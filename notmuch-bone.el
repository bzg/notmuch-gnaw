;;; notmuch-bone.el --- Highlight BARK reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail
;; URL: https://codeberg.org/bzg/notmuch-bone
;; Version: 0.8.0
;; Package-Requires: ((emacs "28.1") (notmuch "0.38") (bone "0.1"))

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
;; M-x bone-update RET          — force update of the remote reports cache
;;
;; The following commands toggle bone's local marks (kept in
;; ~/.config/bone/state.edn so they are shared with the bone CLI):
;;
;; M-x notmuch-bone-mark-sticky RET — toggle the sticky mark (keep visible)
;; M-x notmuch-bone-mark-skip RET — toggle the skip mark (hide)
;;
;; The annotation gains a leading mark column: '*' = sticky, '_' = skip.
;;
;; notmuch-bone builds on the `bone' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the notmuch presentation and commands.
;;
;;; Code:

(require 'bone)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'notmuch)
(require 'notmuch-tree)

(defgroup notmuch-bone nil
  "Highlight BARK reports in notmuch."
  :group 'notmuch)

(defface notmuch-bone-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BARK reports in notmuch search."
  :group 'notmuch-bone)

(defface notmuch-bone-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'notmuch-bone)

(defvar notmuch-bone-votes-width 5
  "Fixed width for the votes column.")

(defvar notmuch-bone-deadline-width 4
  "Fixed width for the deadline column.")

(defvar notmuch-bone-expiry-width 4
  "Fixed width for the expiry column.")

;;; --- State mark prefix ----------------------------------------------------

(defun notmuch-bone--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond
     ((eq flag :sticky) "*")
     (skip            "_")
     (t               " "))))

;; --- Annotation formatting ------------------------------------------------

(defun notmuch-bone--type-letter (type)
  "Get letter abbreviation for TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun notmuch-bone--deadline-days (deadline)
  "Days until YYYY-MM-DD DEADLINE."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun notmuch-bone--annotation (info &optional entry)
  "Build annotation string for report INFO and state ENTRY."
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

;; --- Query building -------------------------------------------------------

(defun notmuch-bone--strip-mid (mid)
  "Strip id: prefix and angle brackets from MID."
  (let ((s (if (string-prefix-p "id:" mid) (substring mid 3) mid)))
    (if (and (string-prefix-p "<" s) (string-suffix-p ">" s))
        (substring s 1 -1)
      s)))

(defun notmuch-bone--build-query (reports)
  "Build query string for REPORTS."
  (mapconcat (lambda (r)
               (format "id:%s" (notmuch-bone--strip-mid (car r))))
             reports
             " or "))

;; --- Overlay highlighting -------------------------------------------------

(defvar-local notmuch-bone--reports nil
  "Buffer-local list of reports for current view.")

(defvar-local notmuch-bone--thread-map nil
  "Buffer-local map from thread-id to (bare-mid . info) for search view.")

(defun notmuch-bone--build-report-map (reports)
  "Build mapping from bare message-id to info for REPORTS."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (notmuch-bone--strip-mid (car r)) (cdr r) ht))
    ht))

(defun notmuch-bone--build-thread-match-map (reports)
  "Map each thread-id to a (BARE-MID . INFO) pair covering REPORTS.
Run notmuch search once to find which thread each report belongs to."
  (let ((thread-map (make-hash-table :test 'equal))
        (report-map (notmuch-bone--build-report-map reports)))
    (when reports
      (let* ((query (notmuch-bone--build-query reports))
             (json-object-type 'alist)
             (json-array-type 'list)
             (threads
              (condition-case nil
                  (with-temp-buffer
                    (call-process "notmuch" nil t nil
                                  "search" "--format=json" query)
                    (goto-char (point-min))
                    (json-read))
                (error nil))))
        (dolist (t-entry threads)
          (let* ((thread-id (concat "thread:" (alist-get 'thread t-entry)))
                 (query-list (car (alist-get 'query t-entry)))
                 (mids (and (stringp query-list)
                            (mapcar (lambda (s) (substring s 3)) ; strip "id:"
                                    (seq-filter
                                     (lambda (s) (string-prefix-p "id:" s))
                                     (split-string query-list))))))
            (when (and thread-id mids)
              (let ((found nil))
                (while (and mids (not found))
                  (let* ((mid (car mids))
                         (info (gethash mid report-map)))
                    (when info
                      (setq found (cons mid info))
                      (puthash thread-id found thread-map)))
                  (setq mids (cdr mids)))))))))
    thread-map))

(defun notmuch-bone--apply-overlays ()
  "Apply overlays in the current notmuch-search or notmuch-tree buffer."
  (remove-overlays (point-min) (point-max) 'notmuch-bone t)
  (when notmuch-bone--reports
    (let ((state (bone-read-state))
          (report-map (notmuch-bone--build-report-map notmuch-bone--reports)))
      (when (and (derived-mode-p 'notmuch-search-mode)
                 (null notmuch-bone--thread-map))
        (setq notmuch-bone--thread-map
              (notmuch-bone--build-thread-match-map notmuch-bone--reports)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((match
                 (cond
                  ((derived-mode-p 'notmuch-tree-mode)
                   (let* ((raw (notmuch-tree-get-message-id t))
                          (mid (and raw (notmuch-bone--strip-mid raw)))
                          (info (and mid (gethash mid report-map))))
                     (and info (cons mid info))))
                  ((derived-mode-p 'notmuch-search-mode)
                   (let ((thread-id (notmuch-search-find-thread-id)))
                     (and thread-id (gethash thread-id notmuch-bone--thread-map)))))))
            (when match
              (let* ((mid     (car match))
                     (info    (cdr match))
                     (entry   (cdr (assoc (bone-normalize-mid mid)
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
  "Clear and re-apply overlays."
  (when (and notmuch-bone--reports
             (or (derived-mode-p 'notmuch-search-mode)
                 (derived-mode-p 'notmuch-tree-mode)))
    (notmuch-bone--apply-overlays)))

;; --- Async: poll until search finishes, then highlight --------------------

(defun notmuch-bone--poll-and-highlight (buffer)
  "Poll BUFFER until notmuch search process finishes, then apply overlays."
  (when (buffer-live-p buffer)
    (let ((proc (get-buffer-process buffer)))
      (if (and proc (process-live-p proc))
          (run-with-timer 0.3 nil #'notmuch-bone--poll-and-highlight buffer)
        (with-current-buffer buffer
          (notmuch-bone--apply-overlays))))))

;; --- Interactive commands -------------------------------------------------

;;;###autoload
(defun notmuch-bone ()
  "Search notmuch for open BARK reports."
  (interactive)
  (let ((reports (bone-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (notmuch-search (notmuch-bone--build-query reports))
      (setq notmuch-bone--reports reports)
      (setq notmuch-bone--thread-map nil)
      (notmuch-bone--poll-and-highlight (current-buffer))
      (message "Searching %d BARK reports." (length reports)))))

;;;###autoload
(defun notmuch-bone-tree ()
  "Show open BARK reports in notmuch tree view."
  (interactive)
  (let ((reports (bone-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (notmuch-tree (notmuch-bone--build-query reports))
      (with-current-buffer (current-buffer)
        (setq notmuch-bone--reports reports)
        (notmuch-bone--poll-and-highlight (current-buffer)))
      (message "Tree view for %d BARK reports." (length reports)))))

;;;###autoload
(defun notmuch-bone-highlight ()
  "Highlight open BARK reports in current notmuch-search buffer."
  (interactive)
  (unless (derived-mode-p 'notmuch-search-mode)
    (user-error "Not in a notmuch-search buffer"))
  (let ((reports (bone-reports)))
    (if (null reports)
        (message "No open BARK reports found.")
      (setq notmuch-bone--reports reports)
      (setq notmuch-bone--thread-map nil)
      (notmuch-bone--apply-overlays)
      (message "Highlighted %d BARK reports." (length reports)))))

(defun notmuch-bone--collect-topics (reports)
  "Sorted list of topics in REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort topics #'string<)))

(defun notmuch-bone--filter-by-topic (reports topic)
  "Return REPORTS matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

;;;###autoload
(defun notmuch-bone-topic ()
  "Search BARK reports filtered by topic."
  (interactive)
  (let* ((reports (bone-reports))
         (topics  (notmuch-bone--collect-topics reports)))
    (cond
     ((null reports) (message "No open BARK reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BARK topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (notmuch-bone--filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (notmuch-search (notmuch-bone--build-query filtered))
          (setq notmuch-bone--reports filtered)
          (setq notmuch-bone--thread-map nil)
          (notmuch-bone--poll-and-highlight (current-buffer))
          (message "Searching %d BARK reports for topic \"%s\"."
                   (length filtered) topic))))))))

;; --- Marking commands -----------------------------------------------------

(defun notmuch-bone--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc (bone-normalize-mid mid) reports)))

(defun notmuch-bone--current-mid (reports)
  "Get current line's bare message-id matching REPORTS."
  (cond
   ((derived-mode-p 'notmuch-tree-mode)
    (let* ((raw (notmuch-tree-get-message-id t))
           (mid (and raw (notmuch-bone--strip-mid raw))))
      (when (and mid (notmuch-bone--info-for-mid mid reports)) mid)))
   ((derived-mode-p 'notmuch-search-mode)
    (let ((thread-id (notmuch-search-find-thread-id)))
      (when thread-id
        (unless notmuch-bone--thread-map
          (setq notmuch-bone--thread-map (notmuch-bone--build-thread-match-map reports)))
        (car (gethash thread-id notmuch-bone--thread-map)))))))

(defun notmuch-bone--mark (action on-msg off-msg)
  "Toggle ACTION mark, showing ON-MSG or OFF-MSG."
  (let* ((reports (or notmuch-bone--reports (bone-reports)))
         (mid     (and reports (notmuch-bone--current-mid reports)))
         (info    (and mid (notmuch-bone--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BARK reports loaded"))
     ((null mid)     (user-error "No BARK report on current line"))
     ((null info)    (user-error "Current line is not a BARK report"))
     (t
      (let ((on (bone-toggle-mark (bone-normalize-mid mid) info action)))
        (notmuch-bone--refresh-overlays)
        (message "%s" (if on on-msg off-msg)))))))

;;;###autoload
(defun notmuch-bone-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (notmuch-bone--mark :sticky "Marked sticky" "Unmarked sticky"))

;;;###autoload
(defun notmuch-bone-mark-skip ()
  "Toggle the skip mark (hide) for the current report."
  (interactive)
  (notmuch-bone--mark :skip "Skipped" "Unskipped"))

;;;###autoload
(defun notmuch-bone-clear ()
  "Remove all notmuch-bone overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'notmuch-bone t)
  (setq notmuch-bone--reports nil)
  (setq notmuch-bone--thread-map nil))

;; --- Cache update hooks ----------------------------------------------------

(defun notmuch-bone--refresh-all-buffers ()
  "Refresh notmuch-bone overlays in all search/tree buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and notmuch-bone--reports
                 (or (derived-mode-p 'notmuch-search-mode)
                     (derived-mode-p 'notmuch-tree-mode)))
        (setq notmuch-bone--thread-map nil)
        (notmuch-bone--apply-overlays)))))

(add-hook 'bone-after-update-hook #'notmuch-bone--refresh-all-buffers)

(provide 'notmuch-bone)
;;; notmuch-bone.el ends here
