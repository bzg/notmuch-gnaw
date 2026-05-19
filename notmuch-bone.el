;;; notmuch-bone.el --- highlight BARK reports in notmuch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail
;; URL: https://codeberg.org/bzg/notmuch-bone

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Show and highlight BARK reports in notmuch.
;;
;; M-x notmuch-bone RET         — search for open BARK reports
;; M-x notmuch-bone-tree RET    — show them in tree view
;; M-x notmuch-bone-topic RET   — search filtered by topic
;; M-x notmuch-bone-highlight RET — highlight matches in current search buffer
;; M-x notmuch-bone-clear RET   — remove highlights
;;
;; The following commands toggle bone's local marks (kept in
;; ~/.config/bone/state.edn so they are shared with the bone CLI):
;;
;; M-x notmuch-bone-mark-read   RET — toggle :read-at on current report
;; M-x notmuch-bone-mark-todo   RET — toggle :todo flag on current report
;; M-x notmuch-bone-mark-sticky RET — toggle :sticky flag on current report
;;
;; The annotation gains a leading mark column: '!' = :todo, '*' = :sticky,
;; 'r' = :read-at (without flag).
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'notmuch)
(require 'notmuch-tree)

(defvar notmuch-bone-config-file "~/.config/bone/config.edn"
  "Path to bone config.edn.
The file is a Clojure/EDN map that may contain:
  :addresses    vector of user email addresses (strings)
  :skip-columns vector of column names to skip (strings)
  :sources      vector of maps, each with a :url key pointing to a
                reports.json URI (local path or http(s) URL).")

(defvar notmuch-bone-state-file "~/.config/bone/state.edn"
  "Path to bone's local state file.
An EDN map keyed by RFC-2822 message-id (with angle brackets).
Each value is a map with keys :subject :type :author :created and
optionally :flag (:todo or :sticky) and :read-at (ISO-8601 string).
The file is shared with the bone CLI; both read and write atomically.")

(defvar notmuch-bone-addresses nil
  "List of user email addresses, populated from `notmuch-bone-config-file'.")

(defface notmuch-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in notmuch search."
  :group 'notmuch-bone)

(defface notmuch-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations (type, flags, priority, votes)."
  :group 'notmuch-bone)

(defconst notmuch-bone-minimum-bark-format "0.9.1"
  "Minimum supported BONE reports.json bark-format.")

(defvar notmuch-bone-votes-width 7
  "Fixed width for the votes column.")

(defvar notmuch-bone-deadline-width 5
  "Fixed width for the deadline column (e.g. \"D-2  \" or \"     \").")

(defvar notmuch-bone-expiry-width 5
  "Fixed width for the expiry column (e.g. \"E-2  \" or \"     \").")

;;; --- Data loading ---

(defun notmuch-bone--uri-to-path (uri)
  "Convert a file:// URI to a local path."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri 7))
    uri))

(defun notmuch-bone--edn-strings-in (key text)
  "Return all double-quoted strings inside the EDN vector for KEY in TEXT.
KEY is a string like \":addresses\".  Returns nil if KEY is absent."
  (when (string-match (concat (regexp-quote key) "[[:space:]]*\\[") text)
    (let ((start (match-end 0))
          (depth 1)
          (i (match-end 0))
          (len (length text))
          end)
      (while (and (< i len) (> depth 0))
        (let ((c (aref text i)))
          (cond ((eq c ?\[) (setq depth (1+ depth)))
                ((eq c ?\]) (setq depth (1- depth)))))
        (setq i (1+ i)))
      (setq end (1- i))
      (let ((sub (substring text start end))
            (acc nil)
            (pos 0))
        (while (string-match "\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" sub pos)
          (push (match-string 1 sub) acc)
          (setq pos (match-end 0)))
        (nreverse acc)))))

(defun notmuch-bone--load-config ()
  "Parse `notmuch-bone-config-file' and return (ADDRESSES . SOURCE-URIS).
SOURCE-URIS is a list of reports.json URIs (file paths or URLs)
extracted from each :url inside the :sources vector."
  (let ((file (expand-file-name notmuch-bone-config-file)))
    (unless (file-readable-p file)
      (error "notmuch-bone: cannot read config %s" file))
    (let* ((text (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
           (addresses (notmuch-bone--edn-strings-in ":addresses" text))
           ;; The :sources vector contains maps; pull every :url "..." inside it.
           (sources-region
            (when (string-match
                   ":sources[[:space:]]*\\[\\(\\(?:[^][]\\|\\[[^][]*\\]\\)*\\)\\]"
                   text)
              (match-string 1 text)))
           (urls nil))
      (when sources-region
        (let ((pos 0))
          (while (string-match
                  ":url[[:space:]]+\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\""
                  sources-region pos)
            (push (match-string 1 sources-region) urls)
            (setq pos (match-end 0)))))
      (cons addresses
            (mapcar #'notmuch-bone--uri-to-path (nreverse urls))))))

(defun notmuch-bone--load-sources ()
  "Return list of reports.json URIs from `notmuch-bone-config-file'.
As a side effect, populates `notmuch-bone-addresses'."
  (let ((cfg (notmuch-bone--load-config)))
    (setq notmuch-bone-addresses (car cfg))
    (cdr cfg)))

(defun notmuch-bone--http-url-p (source)
  "Return non-nil if SOURCE is an HTTP(S) URL."
  (string-match-p "\\`https?://" source))

(defun notmuch-bone--read-json (source)
  "Read JSON from SOURCE, a local path or HTTP(S) URL."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (if (notmuch-bone--http-url-p source)
        (let ((buf (url-retrieve-synchronously source t)))
          (unless buf (error "notmuch-bone: failed to fetch %s" source))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (unless (re-search-forward "\n\n" nil t)
                  (error "notmuch-bone: malformed HTTP response from %s" source))
                (json-read))
            (kill-buffer buf)))
      (json-read-file source))))

(defun notmuch-bone--extract-open-reports (source)
  "Extract open reports from SOURCE (local path or HTTP URL).
Each entry is (MESSAGE-ID . (:type T :flags F :priority P :votes V
:deadline D :expiry E :last-activity LA :topic TOP)).
A report is open when its status is >= 4."
  (let* ((data (notmuch-bone--read-json source))
         (fv (alist-get 'bark-format data))
         (reports (alist-get 'reports data))
         (result '()))
    (when (and fv (version< fv notmuch-bone-minimum-bark-format))
      (message "notmuch-bone: %s has bark-format %s, minimum supported is %s"
               source fv notmuch-bone-minimum-bark-format))
    (dolist (r reports result)
      (let ((mid            (alist-get 'message-id r))
            (status         (alist-get 'status r))
            (type           (alist-get 'type r))
            (acked          (alist-get 'acked r))
            (owned          (alist-get 'owned r))
            (closed         (alist-get 'closed r))
            (close-reason   (alist-get 'close-reason r))
            (priority       (alist-get 'priority r))
            (votes          (alist-get 'votes r))
            (deadline       (alist-get 'deadline r))
            (expiry         (alist-get 'expiry r))
            (last-activity  (alist-get 'last-activity r))
            (topic          (alist-get 'topic r))
            (subject        (alist-get 'subject r))
            (from           (alist-get 'from r))
            (from-name      (alist-get 'from-name r))
            (date           (alist-get 'date r)))
        (when (and mid (numberp status) (>= status 4))
          (let ((flags (concat (if acked "A" "-")
                               (if owned "O" "-")
                               (pcase close-reason
                                 ("canceled"   "C")
                                 ("resolved"   "R")
                                 ("expired"    "E")
                                 ("superseded" "S")
                                 (_ (if closed "R" "-"))))))
            (push (cons mid (list :type (or type "bug")
                                  :flags flags
                                  :priority (or priority 0)
                                  :votes votes
                                  :deadline deadline
                                  :expiry expiry
                                  :last-activity last-activity
                                  :topic topic
                                  :subject subject
                                  :from from
                                  :from-name from-name
                                  :date date))
                  result)))))))

(defun notmuch-bone--load-all-open-reports ()
  "Collect open (message-id . plist) pairs from all sources."
  (mapcan #'notmuch-bone--extract-open-reports
          (notmuch-bone--load-sources)))

;;; --- Minimal EDN reader/writer for ~/.config/bone/state.edn ---
;;
;; The state file is a top-level map: string keys (message-ids) → maps
;; with keyword keys (:subject :type :author :created :read-at :flag).
;; We parse into alists.

(defun notmuch-bone--edn-skip-ws ()
  (skip-chars-forward " \t\n\r,"))

(defun notmuch-bone--edn-read ()
  "Read one EDN value at point."
  (notmuch-bone--edn-skip-ws)
  (let ((c (char-after)))
    (cond
     ((null c)   (error "notmuch-bone EDN: unexpected EOF"))
     ((eq c ?\") (notmuch-bone--edn-read-string))
     ((eq c ?:)  (notmuch-bone--edn-read-keyword))
     ((eq c ?\{) (notmuch-bone--edn-read-map))
     ((eq c ?\[) (notmuch-bone--edn-read-vector))
     ((or (and (>= c ?0) (<= c ?9))
          (and (eq c ?-) (let ((d (char-after (1+ (point)))))
                           (and d (>= d ?0) (<= d ?9)))))
      (notmuch-bone--edn-read-number))
     (t (notmuch-bone--edn-read-symbol)))))

(defun notmuch-bone--edn-read-string ()
  (forward-char 1)
  (let ((chars nil))
    (while (not (eq (char-after) ?\"))
      (let ((c (char-after)))
        (cond
         ((null c) (error "notmuch-bone EDN: unterminated string"))
         ((eq c ?\\)
          (forward-char 1)
          (let ((esc (char-after)))
            (unless esc (error "notmuch-bone EDN: dangling backslash"))
            (push (pcase esc
                    (?n ?\n) (?t ?\t) (?r ?\r)
                    (?b ?\b) (?f ?\f)
                    (?\\ ?\\) (?\" ?\")
                    (?u (forward-char 1)
                        (let ((hex (buffer-substring-no-properties
                                    (point) (+ (point) 4))))
                          (forward-char 3)
                          (string-to-number hex 16)))
                    (_ esc))
                  chars))
          (forward-char 1))
         (t (push c chars) (forward-char 1)))))
    (forward-char 1)
    (apply #'string (nreverse chars))))

(defun notmuch-bone--edn-read-keyword ()
  (forward-char 1)
  (let ((start (1- (point))))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (intern (buffer-substring-no-properties start (point)))))

(defun notmuch-bone--edn-read-symbol ()
  (let ((start (point)))
    (skip-chars-forward "a-zA-Z0-9._/?!+*<>=&%$-")
    (pcase (buffer-substring-no-properties start (point))
      ("nil"   nil)
      ("true"  t)
      ("false" nil)
      (s       (intern s)))))

(defun notmuch-bone--edn-read-number ()
  (let ((start (point)))
    (skip-chars-forward "0-9.eE+-")
    (string-to-number (buffer-substring-no-properties start (point)))))

(defun notmuch-bone--edn-read-map ()
  (forward-char 1)
  (let ((acc nil))
    (notmuch-bone--edn-skip-ws)
    (while (not (eq (char-after) ?\}))
      (let ((k (notmuch-bone--edn-read)))
        (notmuch-bone--edn-skip-ws)
        (push (cons k (notmuch-bone--edn-read)) acc))
      (notmuch-bone--edn-skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun notmuch-bone--edn-read-vector ()
  (forward-char 1)
  (let ((acc nil))
    (notmuch-bone--edn-skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (notmuch-bone--edn-read) acc)
      (notmuch-bone--edn-skip-ws))
    (forward-char 1)
    (nreverse acc)))

(defun notmuch-bone--edn-write-string (s)
  "Serialize S as an EDN/Clojure string literal."
  (concat "\""
          (replace-regexp-in-string
           "[\\\\\"\n\t\r\b\f]"
           (lambda (m)
             (pcase (aref m 0)
               (?\n "\\n") (?\t "\\t") (?\r "\\r")
               (?\b "\\b") (?\f "\\f")
               (?\\ "\\\\") (?\" "\\\"")))
           s t t)
          "\""))

(defun notmuch-bone--edn-write-value (v)
  (cond
   ((stringp v)  (notmuch-bone--edn-write-string v))
   ((keywordp v) (symbol-name v))
   ((eq v t)     "true")
   ((null v)     "nil")
   ((numberp v)  (number-to-string v))
   ((consp v)    (notmuch-bone--edn-write-entry v))
   (t (error "notmuch-bone EDN: cannot serialize %S" v))))

(defun notmuch-bone--edn-write-entry (entry)
  "Format an inner state ENTRY (alist with keyword keys) as an EDN map."
  (if (null entry) "{}"
    (concat "{"
            (mapconcat (lambda (kv)
                         (concat (notmuch-bone--edn-write-value (car kv))
                                 " "
                                 (notmuch-bone--edn-write-value (cdr kv))))
                       entry ", ")
            "}")))

;;; --- State file I/O ---

(defun notmuch-bone--read-state ()
  "Return bone's state.edn as an alist of (mid . inner-alist), or nil."
  (let ((file (expand-file-name notmuch-bone-state-file)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (notmuch-bone--edn-skip-ws)
            (when (eq (char-after) ?{)
              (notmuch-bone--edn-read-map)))
        (error
         (message "notmuch-bone: cannot parse %s: %s — using empty state."
                  file (error-message-string err))
         nil)))))

(defun notmuch-bone--write-state (state)
  "Write STATE (alist of (mid . inner-alist)) to `notmuch-bone-state-file'.
Matches bone's on-disk format: outer map with one entry per line."
  (let ((file (expand-file-name notmuch-bone-state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (if (null state)
          (insert "{}\n")
        (insert "{")
        (let ((first t))
          (dolist (kv state)
            (if first (setq first nil) (insert "\n "))
            (insert (notmuch-bone--edn-write-string (car kv)))
            (insert " ")
            (insert (notmuch-bone--edn-write-entry (cdr kv)))))
        (insert "}\n")))))

;;; --- State transitions (mirror bone's apply-transition) ---

(defun notmuch-bone--iso-now ()
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ" nil t))

(defun notmuch-bone--author-string (info)
  "Build \"Name <email>\" from INFO's :from-name and :from, or nil."
  (let ((n (plist-get info :from-name))
        (e (plist-get info :from)))
    (cond
     ((and n e (not (string-empty-p n))) (concat n " <" e ">"))
     (e e)
     (n n))))

(defun notmuch-bone--alist-dissoc (alist key)
  "Return a copy of ALIST without KEY."
  (assq-delete-all key (copy-alist alist)))

(defun notmuch-bone--alist-assoc (alist key value)
  "Return a copy of ALIST with KEY set to VALUE."
  (let ((e (copy-alist alist)))
    (setf (alist-get key e) value)
    e))

(defun notmuch-bone--enrich-entry (existing info)
  "Refresh entry metadata from INFO plist.  Only sets fields present in INFO.
EXISTING is an alist (keyword keys).  Returns a new alist."
  (let ((entry (copy-alist existing)))
    (dolist (pair '((:subject . :subject)
                    (:type    . :type)
                    (:date    . :created)))
      (when-let* ((v (plist-get info (car pair))))
        (setf (alist-get (cdr pair) entry) v)))
    (when-let* ((author (notmuch-bone--author-string info)))
      (setf (alist-get :author entry) author))
    entry))

(defun notmuch-bone--state-put (state mid entry)
  "Set MID → ENTRY in STATE, preserving order when MID is already present."
  (if (assoc mid state)
      (mapcar (lambda (kv) (if (equal (car kv) mid) (cons mid entry) kv))
              state)
    (append state (list (cons mid entry)))))

(defun notmuch-bone--state-delete (state mid)
  "Remove MID from STATE."
  (cl-remove mid state :key #'car :test #'equal))

(defun notmuch-bone--apply-transition (state action mid info)
  "Toggle ACTION (:read, :todo, :sticky) for MID in STATE.
INFO is the report's plist (used to enrich the entry on first touch).
Mirrors the semantics of bone's `apply-transition'."
  (let* ((base (notmuch-bone--enrich-entry (cdr (assoc mid state)) info))
         (flag (alist-get :flag base))
         (new
          (pcase action
            (:read   (if (alist-get :read-at base)
                         (notmuch-bone--alist-dissoc base :read-at)
                       (notmuch-bone--alist-assoc  base :read-at
                                                   (notmuch-bone--iso-now))))
            (:todo   (if (eq flag :todo)
                         (notmuch-bone--alist-dissoc base :flag)
                       (notmuch-bone--alist-assoc  base :flag :todo)))
            (:sticky (if (eq flag :sticky)
                         (notmuch-bone--alist-dissoc base :flag)
                       (notmuch-bone--alist-assoc  base :flag :sticky))))))
    (if (and (null (alist-get :flag    new))
             (null (alist-get :read-at new)))
        (notmuch-bone--state-delete state mid)
      (notmuch-bone--state-put state mid new))))

(defun notmuch-bone--mark-prefix (entry)
  "Return a single-character mark for state ENTRY.
\"!\" for :todo, \"*\" for :sticky, \"r\" for :read-at (no flag),
space otherwise."
  (let ((flag (cdr (assq :flag entry)))
        (read (cdr (assq :read-at entry))))
    (cond
     ((eq flag :todo)   "!")
     ((eq flag :sticky) "*")
     (read              "r")
     (t                 " "))))

(defun notmuch-bone--action-on-p (state mid action)
  "Return non-nil when ACTION is set for MID in STATE."
  (let ((entry (cdr (assoc mid state))))
    (pcase action
      (:read   (cdr (assq :read-at entry)))
      (:todo   (eq (cdr (assq :flag entry)) :todo))
      (:sticky (eq (cdr (assq :flag entry)) :sticky)))))

;;; --- Annotation formatting ---

(defun notmuch-bone--type-letter (type)
  "Return a single-letter abbreviation for report TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun notmuch-bone--deadline-days (deadline)
  "Return days until DEADLINE (a \"YYYY-MM-DD\" string), or nil."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun notmuch-bone--annotation (info &optional entry)
  "Build a fixed-width annotation string from report INFO plist.
When non-nil, ENTRY is the state.edn alist for this report; its
mark prefix (':todo', ':sticky', ':read-at') is shown as the
leftmost column."
  (let* ((mark     (notmuch-bone--mark-prefix entry))
         (type     (notmuch-bone--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (expiry   (plist-get info :expiry))
         (dl-days  (notmuch-bone--deadline-days deadline))
         (ex-days  (notmuch-bone--deadline-days expiry))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if dl-days (format "D%+d" dl-days) ""))
         (dl-pad   (string-pad dl-str notmuch-bone-deadline-width))
         (ex-str   (if ex-days (format "E%+d" ex-days) ""))
         (ex-pad   (string-pad ex-str notmuch-bone-expiry-width))
         (votes-str (if votes (format "[%s]" votes) ""))
         (votes-pad (string-pad votes-str notmuch-bone-votes-width))
         (tag      (concat mark " " type " " flags " " pri-str " "
                           dl-pad ex-pad votes-pad)))
    tag))

;;; --- Query building ---

(defun notmuch-bone--strip-mid (mid)
  "Normalize MID by removing angle brackets and \"id:\" prefix."
  (let ((s (if (string-prefix-p "id:" mid) (substring mid 3) mid)))
    (if (and (string-prefix-p "<" s) (string-suffix-p ">" s))
        (substring s 1 -1)
      s)))

(defun notmuch-bone--build-query (reports)
  "Build a notmuch query matching all message-ids in REPORTS.
REPORTS is a list of (message-id . plist)."
  (mapconcat (lambda (r)
               (format "id:%s" (notmuch-bone--strip-mid (car r))))
             reports
             " or "))

;;; --- Overlay highlighting ---

(defun notmuch-bone--bracket-mid (bare)
  "Return BARE message-id wrapped in angle brackets."
  (if (and (string-prefix-p "<" bare) (string-suffix-p ">" bare))
      bare
    (concat "<" bare ">")))

(defvar-local notmuch-bone--reports nil
  "Buffer-local list of (bare-message-id . info-plist) for current view.")

(defun notmuch-bone--build-report-map (reports)
  "Build a hash-table from REPORTS for fast lookup by bare message-id."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (notmuch-bone--strip-mid (car r)) (cdr r) ht))
    ht))

(defun notmuch-bone--thread-message-ids (thread-id)
  "Return list of bare message-ids for THREAD-ID via notmuch search."
  (let ((output (with-temp-buffer
                  (call-process "notmuch" nil t nil
                                "search" "--output=messages" thread-id)
                  (buffer-string))))
    (mapcar #'notmuch-bone--strip-mid
            (split-string (string-trim output) "\n" t))))

(defun notmuch-bone--find-match-in-thread (thread-id report-map)
  "Return (BARE-MID . INFO) for the first matching message in THREAD-ID, or nil."
  (let ((mids (notmuch-bone--thread-message-ids thread-id))
        (found nil))
    (while (and mids (not found))
      (let ((info (gethash (car mids) report-map)))
        (when info (setq found (cons (car mids) info))))
      (setq mids (cdr mids)))
    found))

(defun notmuch-bone--apply-overlays ()
  "Apply overlays in the current notmuch-search buffer.
Walk each result line; if any message-id in that thread matches a
report, highlight it and prepend an annotation via `before-string'."
  (when notmuch-bone--reports
    (let ((report-map (notmuch-bone--build-report-map notmuch-bone--reports))
          (state      (notmuch-bone--read-state)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((thread-id (notmuch-search-find-thread-id))
                 (match (and thread-id
                             (notmuch-bone--find-match-in-thread
                              thread-id report-map))))
            (when match
              (let* ((mid     (car match))
                     (info    (cdr match))
                     (entry   (cdr (assoc (notmuch-bone--bracket-mid mid)
                                          state)))
                     (bol     (line-beginning-position))
                     (eol     (line-end-position))
                     (ann-str (notmuch-bone--annotation info entry))
                     (p3      (= 3 (plist-get info :priority)))
                     (ov      (make-overlay bol eol)))
                (when p3 (overlay-put ov 'face 'bold))
                (overlay-put ov 'notmuch-bone t)
                (overlay-put ov 'before-string
                             (propertize (concat ann-str " ")
                                         'face 'notmuch-bone-annotation-face)))))
          (forward-line 1))))))

(defun notmuch-bone--refresh-overlays ()
  "Clear and re-apply notmuch-bone overlays in the current search buffer."
  (when (and notmuch-bone--reports
             (derived-mode-p 'notmuch-search-mode))
    (remove-overlays (point-min) (point-max) 'notmuch-bone t)
    (notmuch-bone--apply-overlays)))

;;; --- Async: poll until search finishes, then highlight ---

(defun notmuch-bone--poll-and-highlight (buffer)
  "Poll BUFFER until the notmuch search process finishes, then apply overlays."
  (when (buffer-live-p buffer)
    (let ((proc (get-buffer-process buffer)))
      (if (and proc (process-live-p proc))
          ;; Still running — check again shortly
          (run-with-timer 0.3 nil #'notmuch-bone--poll-and-highlight buffer)
        ;; Process done — apply overlays
        (with-current-buffer buffer
          (notmuch-bone--apply-overlays))))))

;;; --- Interactive commands ---

;;;###autoload
(defun notmuch-bone ()
  "Search notmuch for open BARK reports."
  (interactive)
  (let ((reports (notmuch-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (notmuch-search (notmuch-bone--build-query reports))
      (setq notmuch-bone--reports reports)
      (notmuch-bone--poll-and-highlight (current-buffer))
      (message "Searching %d BARK reports." (length reports)))))

;;;###autoload
(defun notmuch-bone-tree ()
  "Show open BARK reports in notmuch tree view."
  (interactive)
  (let ((reports (notmuch-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (notmuch-tree (notmuch-bone--build-query reports))
      ;; `notmuch-tree' leaves the new tree buffer as the current buffer
      ;; (via `pop-to-buffer-same-window'); rely on that so the buffer-
      ;; local cache used by marking commands lands on the right buffer.
      (with-current-buffer (current-buffer)
        (setq notmuch-bone--reports reports))
      (message "Tree view for %d BARK reports." (length reports)))))

;;;###autoload
(defun notmuch-bone-highlight ()
  "Highlight lines in the current notmuch-search buffer that match BARK reports.
Use `notmuch-bone-clear' to remove highlights."
  (interactive)
  (unless (derived-mode-p 'notmuch-search-mode)
    (user-error "Not in a notmuch-search buffer"))
  (let ((reports (notmuch-bone--load-all-open-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (setq notmuch-bone--reports reports)
      (notmuch-bone--apply-overlays)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun notmuch-bone--collect-topics (reports)
  "Return sorted list of unique topics from REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (when-let ((topic (plist-get (cdr r) :topic)))
        (cl-pushnew topic topics :test #'equal)))
    (sort topics #'string<)))

(defun notmuch-bone--filter-by-topic (reports topic)
  "Return REPORTS whose :topic equals TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                     reports))

;;;###autoload
(defun notmuch-bone-topic ()
  "Like `notmuch-bone', but limited to a single topic."
  (interactive)
  (let* ((reports (notmuch-bone--load-all-open-reports))
         (topics  (notmuch-bone--collect-topics reports))
         (topic   (completing-read "BARK topic: " topics nil t)))
    (if (string-empty-p topic)
        (message "No topic selected.")
      (let ((filtered (notmuch-bone--filter-by-topic reports topic)))
        (if (null filtered)
            (message "No reports for topic \"%s\"." topic)
          (notmuch-search (notmuch-bone--build-query filtered))
          (setq notmuch-bone--reports filtered)
          (notmuch-bone--poll-and-highlight (current-buffer))
          (message "Searching %d BARK reports for topic \"%s\"."
                   (length filtered) topic))))))

;;; --- Marking commands ---

(defun notmuch-bone--info-for-mid (mid reports)
  "Return the info plist for bare MID in REPORTS, or nil."
  (catch 'found
    (dolist (r reports)
      (when (equal (notmuch-bone--strip-mid (car r)) mid)
        (throw 'found (cdr r))))))

(defun notmuch-bone--current-mid (reports)
  "Return current line's bare message-id matching one in REPORTS, or nil.
In `notmuch-tree-mode' uses the current message-id; in
`notmuch-search-mode' returns the first message-id in the thread
that is also a BARK report."
  (cond
   ((derived-mode-p 'notmuch-tree-mode)
    (when-let* ((raw (notmuch-tree-get-message-id t))
                (mid (notmuch-bone--strip-mid raw)))
      (when (notmuch-bone--info-for-mid mid reports) mid)))
   ((derived-mode-p 'notmuch-search-mode)
    (when-let* ((report-map (notmuch-bone--build-report-map reports))
                (thread-id  (notmuch-search-find-thread-id))
                (match      (notmuch-bone--find-match-in-thread
                             thread-id report-map)))
      (car match)))))

(defun notmuch-bone--mark (action on-msg off-msg)
  "Toggle ACTION on the current BARK report.
Show ON-MSG or OFF-MSG depending on the resulting state."
  (let* ((reports (or notmuch-bone--reports
                      (notmuch-bone--load-all-open-reports)))
         (mid     (and reports (notmuch-bone--current-mid reports)))
         (info    (and mid (notmuch-bone--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BARK reports loaded"))
     ((null mid)     (user-error "No BARK report on current line"))
     ((null info)    (user-error "Current line is not a BARK report"))
     (t
      (let* ((bracketed (notmuch-bone--bracket-mid mid))
             (state     (notmuch-bone--read-state))
             (new       (notmuch-bone--apply-transition
                         state action bracketed info)))
        (notmuch-bone--write-state new)
        (notmuch-bone--refresh-overlays)
        (message "%s" (if (notmuch-bone--action-on-p new bracketed action)
                          on-msg off-msg)))))))

;;;###autoload
(defun notmuch-bone-mark-read ()
  "Toggle the :read-at timestamp for the current BARK report.
Edits bone's `~/.config/bone/state.edn'.

In `notmuch-search-mode' the current line is a thread; the *first*
message-id in that thread that is also a BARK report is the one
toggled.  Use `notmuch-bone-tree' (or open the thread) to mark a
specific report when a thread contains several."
  (interactive)
  (notmuch-bone--mark :read "Marked read" "Unmarked read"))

;;;###autoload
(defun notmuch-bone-mark-todo ()
  "Toggle the :todo flag for the current BARK report.
Edits bone's `~/.config/bone/state.edn'.

See `notmuch-bone-mark-read' for the search-mode caveat about
threads containing several BARK reports."
  (interactive)
  (notmuch-bone--mark :todo "Marked TODO" "Unmarked TODO"))

;;;###autoload
(defun notmuch-bone-mark-sticky ()
  "Toggle the :sticky flag for the current BARK report.
Edits bone's `~/.config/bone/state.edn'.

See `notmuch-bone-mark-read' for the search-mode caveat about
threads containing several BARK reports."
  (interactive)
  (notmuch-bone--mark :sticky "Marked STICKY" "Unmarked STICKY"))

;;;###autoload
(defun notmuch-bone-clear ()
  "Remove all notmuch-bone overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'notmuch-bone t)
  (setq notmuch-bone--reports nil))

(provide 'notmuch-bone)
;;; notmuch-bone.el ends here
