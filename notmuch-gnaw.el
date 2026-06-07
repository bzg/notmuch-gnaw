;;; notmuch-gnaw.el --- Highlight BONE reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: mail
;; URL: https://codeberg.org/bzg/notmuch-gnaw
;; Version: 0.8.0
;; Package-Requires: ((emacs "28.1") (notmuch "0.38") (gnaw "0.1"))

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
;; Show and highlight BONE reports in notmuch.
;;
;; M-x notmuch-gnaw RET         — search for open BONE reports
;; M-x notmuch-gnaw-tree RET    — show them in tree view
;; M-x notmuch-gnaw-topic RET   — search filtered by topic
;; M-x notmuch-gnaw-highlight RET — highlight matches in current search buffer
;; M-x notmuch-gnaw-clear RET   — remove highlights
;; M-x gnaw-update RET          — force update of the remote reports cache
;;
;; The following commands toggle gnaw's local marks (kept in
;; ~/.config/gnaw/state.edn so they are shared with the gnaw CLI):
;;
;; M-x notmuch-gnaw-mark-sticky RET — toggle the sticky mark (keep visible)
;; M-x notmuch-gnaw-mark-skip RET — toggle the skip mark (hide)
;;
;; The annotation gains a leading mark column: '*' = sticky, '_' = skip.
;;
;; notmuch-gnaw builds on the `gnaw' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the notmuch presentation and commands.
;;
;;; Code:

(require 'gnaw)
(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'notmuch)
(require 'notmuch-tree)

(defgroup notmuch-gnaw nil
  "Highlight BONE reports in notmuch."
  :group 'notmuch)

(defface notmuch-gnaw-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BONE reports in notmuch search."
  :group 'notmuch-gnaw)

(defface notmuch-gnaw-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'notmuch-gnaw)

(defvar notmuch-gnaw-votes-width 5
  "Fixed width for the votes column.")

(defvar notmuch-gnaw-deadline-width 4
  "Fixed width for the deadline column.")

(defvar notmuch-gnaw-expiry-width 4
  "Fixed width for the expiry column.")

;;; --- State mark prefix ----------------------------------------------------

(defun notmuch-gnaw--mark-prefix (entry)
  "Get mark char for state ENTRY."
  (let ((flag (cdr (assq :flag entry)))
        (skip (cdr (assq :skip-since entry))))
    (cond
     ((eq flag :sticky) "*")
     (skip            "_")
     (t               " "))))

;; --- Annotation formatting ------------------------------------------------

(defun notmuch-gnaw--type-letter (type)
  "Get letter abbreviation for TYPE."
  (pcase type
    ("bug"          "B")
    ("patch"        "P")
    ("request"      "?")
    ("announcement" "A")
    ("release"      "R")
    ("change"       "C")
    (_              "·")))

(defun notmuch-gnaw--deadline-days (deadline)
  "Days until YYYY-MM-DD DEADLINE."
  (when deadline
    (let* ((dl (date-to-time (concat deadline " 00:00:00")))
           (diff (float-time (time-subtract dl (current-time)))))
      (ceiling (/ diff 86400.0)))))

(defun notmuch-gnaw--annotation (info &optional entry)
  "Build annotation string for report INFO and state ENTRY."
  (let* ((mark     (notmuch-gnaw--mark-prefix entry))
         (type     (notmuch-gnaw--type-letter (plist-get info :type)))
         (flags    (plist-get info :flags))
         (priority (plist-get info :priority))
         (votes    (plist-get info :votes))
         (deadline (plist-get info :deadline))
         (expiry   (plist-get info :expiry))
         (dl-days  (notmuch-gnaw--deadline-days deadline))
         (ex-days  (notmuch-gnaw--deadline-days expiry))
         (pri-str  (pcase priority (3 "A") (2 "B") (1 "C") (_ " ")))
         (dl-str   (if dl-days (format "D%+d" dl-days) ""))
         (dl-pad   (string-pad dl-str notmuch-gnaw-deadline-width))
         (ex-str   (if ex-days (format "E%+d" ex-days) ""))
         (ex-pad   (string-pad ex-str notmuch-gnaw-expiry-width))
         (votes-str (if votes (format "[%s]" votes) ""))
         (votes-pad (string-pad votes-str notmuch-gnaw-votes-width))
         (tag      (concat mark " " type " " flags " " pri-str " "
                           dl-pad ex-pad votes-pad)))
    tag))

;; --- Query building -------------------------------------------------------

(defun notmuch-gnaw--strip-mid (mid)
  "Strip id: prefix and angle brackets from MID."
  (let ((s (if (string-prefix-p "id:" mid) (substring mid 3) mid)))
    (if (and (string-prefix-p "<" s) (string-suffix-p ">" s))
        (substring s 1 -1)
      s)))

(defun notmuch-gnaw--build-query (reports)
  "Build query string for REPORTS."
  (mapconcat (lambda (r)
               (format "id:%s" (notmuch-gnaw--strip-mid (car r))))
             reports
             " or "))

;; --- Overlay highlighting -------------------------------------------------

(defvar-local notmuch-gnaw--reports nil
  "Buffer-local list of reports for current view.")

(defvar-local notmuch-gnaw--thread-map nil
  "Buffer-local map from thread-id to (bare-mid . info) for search view.")

(defun notmuch-gnaw--build-report-map (reports)
  "Build mapping from bare message-id to info for REPORTS."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (notmuch-gnaw--strip-mid (car r)) (cdr r) ht))
    ht))

(defun notmuch-gnaw--build-thread-match-map (reports)
  "Map each thread-id to a (BARE-MID . INFO) pair covering REPORTS.
Run notmuch search once to find which thread each report belongs to."
  (let ((thread-map (make-hash-table :test 'equal))
        (report-map (notmuch-gnaw--build-report-map reports)))
    (when reports
      (let* ((query (notmuch-gnaw--build-query reports))
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

(defun notmuch-gnaw--apply-overlays ()
  "Apply overlays in the current notmuch-search or notmuch-tree buffer."
  (remove-overlays (point-min) (point-max) 'notmuch-gnaw t)
  (when notmuch-gnaw--reports
    (let ((state (gnaw-read-state))
          (report-map (notmuch-gnaw--build-report-map notmuch-gnaw--reports)))
      (when (and (derived-mode-p 'notmuch-search-mode)
                 (null notmuch-gnaw--thread-map))
        (setq notmuch-gnaw--thread-map
              (notmuch-gnaw--build-thread-match-map notmuch-gnaw--reports)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((match
                 (cond
                  ((derived-mode-p 'notmuch-tree-mode)
                   (let* ((raw (notmuch-tree-get-message-id t))
                          (mid (and raw (notmuch-gnaw--strip-mid raw)))
                          (info (and mid (gethash mid report-map))))
                     (and info (cons mid info))))
                  ((derived-mode-p 'notmuch-search-mode)
                   (let ((thread-id (notmuch-search-find-thread-id)))
                     (and thread-id (gethash thread-id notmuch-gnaw--thread-map)))))))
            (when match
              (let* ((mid     (car match))
                     (info    (cdr match))
                     (entry   (cdr (assoc (gnaw-normalize-mid mid)
                                          state)))
                     (bol     (line-beginning-position))
                     (eol     (line-end-position))
                     (ann-str (notmuch-gnaw--annotation info entry))
                     (p3      (= 3 (plist-get info :priority)))
                     (ov      (make-overlay bol eol)))
                (when p3 (overlay-put ov 'face 'bold))
                (overlay-put ov 'notmuch-gnaw t)
                (overlay-put ov 'before-string
                             (propertize (concat ann-str " ")
                                         'face 'notmuch-gnaw-annotation-face)))))
          (forward-line 1))))))

(defun notmuch-gnaw--refresh-overlays ()
  "Clear and re-apply overlays."
  (when (and notmuch-gnaw--reports
             (or (derived-mode-p 'notmuch-search-mode)
                 (derived-mode-p 'notmuch-tree-mode)))
    (notmuch-gnaw--apply-overlays)))

;; --- Async: poll until search finishes, then highlight --------------------

(defun notmuch-gnaw--poll-and-highlight (buffer)
  "Poll BUFFER until notmuch search process finishes, then apply overlays."
  (when (buffer-live-p buffer)
    (let ((proc (get-buffer-process buffer)))
      (if (and proc (process-live-p proc))
          (run-with-timer 0.3 nil #'notmuch-gnaw--poll-and-highlight buffer)
        (with-current-buffer buffer
          (notmuch-gnaw--apply-overlays))))))

;; --- Interactive commands -------------------------------------------------

;;;###autoload
(defun notmuch-gnaw ()
  "Search notmuch for open BONE reports."
  (interactive)
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (notmuch-search (notmuch-gnaw--build-query reports))
      (setq notmuch-gnaw--reports reports)
      (setq notmuch-gnaw--thread-map nil)
      (notmuch-gnaw--poll-and-highlight (current-buffer))
      (message "Searching %d BONE reports." (length reports)))))

;;;###autoload
(defun notmuch-gnaw-tree ()
  "Show open BONE reports in notmuch tree view."
  (interactive)
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (notmuch-tree (notmuch-gnaw--build-query reports))
      (with-current-buffer (current-buffer)
        (setq notmuch-gnaw--reports reports)
        (notmuch-gnaw--poll-and-highlight (current-buffer)))
      (message "Tree view for %d BONE reports." (length reports)))))

;;;###autoload
(defun notmuch-gnaw-highlight ()
  "Highlight open BONE reports in current notmuch-search buffer."
  (interactive)
  (unless (derived-mode-p 'notmuch-search-mode)
    (user-error "Not in a notmuch-search buffer"))
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (setq notmuch-gnaw--reports reports)
      (setq notmuch-gnaw--thread-map nil)
      (notmuch-gnaw--apply-overlays)
      (message "Highlighted %d BONE reports." (length reports)))))

(defun notmuch-gnaw--collect-topics (reports)
  "Sorted list of topics in REPORTS."
  (let ((topics nil))
    (dolist (r reports)
      (let ((topic (plist-get (cdr r) :topic)))
        (when topic
          (cl-pushnew topic topics :test #'equal))))
    (sort topics #'string<)))

(defun notmuch-gnaw--filter-by-topic (reports topic)
  "Return REPORTS matching TOPIC."
  (cl-remove-if-not (lambda (r) (equal (plist-get (cdr r) :topic) topic))
                    reports))

;;;###autoload
(defun notmuch-gnaw-topic ()
  "Search BONE reports filtered by topic."
  (interactive)
  (let* ((reports (gnaw-reports))
         (topics  (notmuch-gnaw--collect-topics reports)))
    (cond
     ((null reports) (message "No open BONE reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BONE topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (notmuch-gnaw--filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (notmuch-search (notmuch-gnaw--build-query filtered))
          (setq notmuch-gnaw--reports filtered)
          (setq notmuch-gnaw--thread-map nil)
          (notmuch-gnaw--poll-and-highlight (current-buffer))
          (message "Searching %d BONE reports for topic \"%s\"."
                   (length filtered) topic))))))))

;; --- Marking commands -----------------------------------------------------

(defun notmuch-gnaw--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc (gnaw-normalize-mid mid) reports)))

(defun notmuch-gnaw--current-mid (reports)
  "Get current line's bare message-id matching REPORTS."
  (cond
   ((derived-mode-p 'notmuch-tree-mode)
    (let* ((raw (notmuch-tree-get-message-id t))
           (mid (and raw (notmuch-gnaw--strip-mid raw))))
      (when (and mid (notmuch-gnaw--info-for-mid mid reports)) mid)))
   ((derived-mode-p 'notmuch-search-mode)
    (let ((thread-id (notmuch-search-find-thread-id)))
      (when thread-id
        (unless notmuch-gnaw--thread-map
          (setq notmuch-gnaw--thread-map (notmuch-gnaw--build-thread-match-map reports)))
        (car (gethash thread-id notmuch-gnaw--thread-map)))))))

(defun notmuch-gnaw--mark (action on-msg off-msg)
  "Toggle ACTION mark, showing ON-MSG or OFF-MSG."
  (let* ((reports (or notmuch-gnaw--reports (gnaw-reports)))
         (mid     (and reports (notmuch-gnaw--current-mid reports)))
         (info    (and mid (notmuch-gnaw--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BONE reports loaded"))
     ((null mid)     (user-error "No BONE report on current line"))
     ((null info)    (user-error "Current line is not a BONE report"))
     (t
      (let ((on (gnaw-toggle-mark (gnaw-normalize-mid mid) info action)))
        (notmuch-gnaw--refresh-overlays)
        (message "%s" (if on on-msg off-msg)))))))

;;;###autoload
(defun notmuch-gnaw-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (notmuch-gnaw--mark :sticky "Marked sticky" "Unmarked sticky"))

;;;###autoload
(defun notmuch-gnaw-mark-skip ()
  "Toggle the skip mark (hide) for the current report."
  (interactive)
  (notmuch-gnaw--mark :skip "Skipped" "Unskipped"))

;;;###autoload
(defun notmuch-gnaw-clear ()
  "Remove all notmuch-gnaw overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'notmuch-gnaw t)
  (setq notmuch-gnaw--reports nil)
  (setq notmuch-gnaw--thread-map nil))

;; --- Cache update hooks ----------------------------------------------------

(defun notmuch-gnaw--refresh-all-buffers ()
  "Refresh notmuch-gnaw overlays in all search/tree buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and notmuch-gnaw--reports
                 (or (derived-mode-p 'notmuch-search-mode)
                     (derived-mode-p 'notmuch-tree-mode)))
        (setq notmuch-gnaw--thread-map nil)
        (notmuch-gnaw--apply-overlays)))))

(add-hook 'gnaw-after-update-hook #'notmuch-gnaw--refresh-all-buffers)

(provide 'notmuch-gnaw)
;;; notmuch-gnaw.el ends here
