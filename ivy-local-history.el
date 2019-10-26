;;; -*- lexical-binding: t; -*-
;;; ivy-local-history --- ivy interface for local-history

;; Author: Alexander Akhmetov <a@alx.cx>
;; Version: 0.1.0
;; Package-Requires: ((local-history "0.1.0"))

;;; Code:
(require 'ivy)

(defun ivy-local-history--select-action (file)
  "Open the selected FILE in the current frame."
  (switch-to-buffer (find-file-noselect (car file))))

;;;###autoload
(defun ivy-local-history ()
  "Show local history for the current buffer."
  (interactive)
  (ivy-read ""
            (local-history--history-files)
            :require-match t
            :action #'ivy-local-history--select-action
            :caller 'ivy-local-history))

(ivy-configure 'ivy-local-history
  :display-transformer-fn #'ivy-local-history--format-row)

(defun ivy-local-history--format-row (match)
  "Convert the MATCH returned by `latest-history-files` into a candidate string."
  (let* ((split (split-string (file-name-nondirectory match) "_"))
         (date (ivy-local-history--format-date split))
         (filename (mapconcat 'identity (seq-subseq split 1) "_")))
    (format "%s    %s" filename date)))


(defun ivy-local-history--format-date (split)
  "Format given SPLIT string into a date string."
  (format "%s" (split-string (car split) "T")))


(provide 'ivy-local-history)
;;; ivy-local-history.el ends here
