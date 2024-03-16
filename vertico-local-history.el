;;; vertico-local-history.el --- Vertico interface for local-history -*- lexical-binding: t; -*-

;; Author: Alexander Akhmetov <me@alx.cx>
;; Version: 0.1.0
;; Package-Requires: ((vertico "0.15.0") (local-history "0.1.0") (emacs "27.1"))

;;; Commentary:
;;
;; This package provides a Vertico interface to browse the file edit history
;; maintained by the `local-history` package.

;;; Code:

(require 'vertico)
(require 'local-history)

(defun vertico-local-history--select-action (file)
  "Open the selected FILE in a new tab."
  (find-file-other-tab file))

(defun vertico-local-history--relative-date (time)
  "Return a human-readable relative time from the given TIME."
  (let* ((current-time (current-time))
         (time-diff (time-subtract current-time time))
         (seconds (time-to-seconds time-diff)))
    (cond
     ((< seconds 60) (format "%d seconds ago" seconds))
     ((< seconds 3600) (format "%d minutes ago" (/ seconds 60)))
     ((< seconds 86400) (format "%d hours ago" (/ seconds 3600)))
     ((< seconds 604800) (format "%d days ago" (/ seconds 86400)))
     ((< seconds 2419200) (format "%d weeks ago" (/ seconds 604800)))
     ((< seconds 29030400) (format "%d months ago" (/ seconds 2419200)))
     (t (format "%d years ago" (/ seconds 29030400))))))

(defun vertico-local-history--format-candidates ()
  "Prepare and format the list of candidates for Vertico from local history."
  (let* ((files (mapcar #'car (local-history--history-files)))
         (files-with-times (mapcar (lambda (f)
                                     (cons f (nth 5 (file-attributes f))))
                                   files)))
    (mapcar
     (lambda (ft)
       (let* ((filepath (car ft))
              (file-time (cdr ft))
              (filename (file-name-nondirectory filepath))
              (date (format-time-string "%Y-%m-%d" file-time))
              (time (format-time-string "%H:%M:%S" file-time))
              (original-filename (mapconcat 'identity (cddr (split-string filename "_")) "_"))
              (relative-date (vertico-local-history--relative-date file-time))
              (date-string (propertize date 'face '(:foreground "LightSkyBlue")))
              (display-string (format "%s %s - %s (%s)" original-filename date-string time relative-date)))
         (cons display-string filepath)))
     files-with-times)))


;;;###autoload
(defun vertico-local-history ()
  "Show local history for the current buffer using Vertico interface."
  (interactive)
  (let ((vertico-sort-function nil))
    (let* ((candidates (vertico-local-history--format-candidates))
           (selection (assoc (completing-read "Choose file version: " candidates) candidates)))
      (when selection
        (vertico-local-history--select-action (cdr selection))))))


(provide 'vertico-local-history)
;;; vertico-local-history.el ends here
