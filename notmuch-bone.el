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
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'notmuch)

(defvar notmuch-bone-config-file "~/.config/bone/config.edn"
  "Path to bone config.edn.
The file is a Clojure/EDN map that may contain:
  :addresses    vector of user email addresses (strings)
  :skip-columns vector of column names to skip (strings)
  :sources      vector of maps, each with a :url key pointing to a
                reports.json URI (local path or http(s) URL).")

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
            (topic          (alist-get 'topic r)))
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
                                  :topic topic))
                  result)))))))

(defun notmuch-bone--load-all-open-reports ()
  "Collect open (message-id . plist) pairs from all sources."
  (mapcan #'notmuch-bone--extract-open-reports
          (notmuch-bone--load-sources)))

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

(defun notmuch-bone--annotation (info)
  "Build a fixed-width annotation string from report INFO plist."
  (let* ((type     (notmuch-bone--type-letter (plist-get info :type)))
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
         (tag      (concat type " " flags " " pri-str " " dl-pad ex-pad votes-pad)))
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

(defun notmuch-bone--normalize-mid (mid)
  "Strip brackets from MID for consistent lookup."
  (notmuch-bone--strip-mid mid))

(defvar-local notmuch-bone--report-map nil
  "Buffer-local hash-table: bare message-id -> info plist.")

(defvar-local notmuch-bone--active nil
  "Non-nil when bone highlighting is active in this buffer.")

(defun notmuch-bone--build-report-map (reports)
  "Build a hash-table from REPORTS for fast lookup by message-id."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (notmuch-bone--normalize-mid (car r)) (cdr r) ht))
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
  "Return first matching report info plist for THREAD-ID, or nil."
  (let ((mids (notmuch-bone--thread-message-ids thread-id))
        (found nil))
    (while (and mids (not found))
      (setq found (gethash (car mids) report-map))
      (setq mids (cdr mids)))
    found))

(defun notmuch-bone--apply-overlays ()
  "Apply overlays in the current notmuch-search buffer.
Walk each result line; if any message-id in that thread matches a
report, highlight it and prepend an annotation via `before-string'."
  (when notmuch-bone--report-map
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((thread-id (notmuch-search-find-thread-id))
               (info (and thread-id
                          (notmuch-bone--find-match-in-thread
                           thread-id notmuch-bone--report-map))))
            (when info
              (let* ((bol (line-beginning-position))
                     (eol (line-end-position))
                     (ann-str (notmuch-bone--annotation info))
                     (p3 (= 3 (plist-get info :priority)))
                     (ov (make-overlay bol eol)))
                (when p3 (overlay-put ov 'face 'bold))
                (overlay-put ov 'notmuch-bone t)
                (overlay-put ov 'before-string
                             (propertize (concat ann-str " ")
                                         'face 'notmuch-bone-annotation-face)))))
          (forward-line 1)))))

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
      (setq notmuch-bone--report-map
            (notmuch-bone--build-report-map reports))
      (setq notmuch-bone--active t)
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
      (setq notmuch-bone--report-map
            (notmuch-bone--build-report-map reports))
      (setq notmuch-bone--active t)
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
          (setq notmuch-bone--report-map
                (notmuch-bone--build-report-map filtered))
          (setq notmuch-bone--active t)
          (notmuch-bone--poll-and-highlight (current-buffer))
          (message "Searching %d BARK reports for topic \"%s\"."
                   (length filtered) topic))))))

;;;###autoload
(defun notmuch-bone-clear ()
  "Remove all notmuch-bone overlays and disable auto-rehighlighting."
  (interactive)
  (remove-overlays (point-min) (point-max) 'notmuch-bone t)
  (setq notmuch-bone--active nil)
  (setq notmuch-bone--report-map nil))

(provide 'notmuch-bone)
;;; notmuch-bone.el ends here
