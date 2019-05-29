;;; local-history.el --- history of all edited files

;;; Commentary:
;;
;; Every time when you save a file this package creates a local copy
;; in the LOCAL-HISTORY-PATH (by default: USER-EMACS-DIRECTORY/local-history)
;;
;; For example, when you edit a file ~/my-project/file.el and save it,
;; it creates a local copy:
;;
;;   ~/.emacs.d/local-history/home/user/my-projects/file.el/<date_and_time>_file.el
;;
;; LOCAL-HISTORY-SAVE-MIN-INTERVAL controls how often files should be saved,
;; by default it creates a copy not more that once per 5 seconds
;;
;;; Code:

(require 'subr-x)


(defvar local-history-path (concat user-emacs-directory "local-history"))
(defvar local-history-save-min-interval 5)


(defun save-file-history (&optional args)
  "Saves current buffer to local history folder (LOCAL-HISTORY-PATH)."
  (when (buffer-file-name)
    (let* ((dir-to-save (concat local-history-path
                                (file-name-as-directory buffer-file-name)))
           (backup-file (get-backup-file-path dir-to-save buffer-file-name)))
      (when (file-needs-to-be-saved-p dir-to-save)
        (let ((command-to-execute (format "mkdir -p %s && cp %s %s"
                                          (shell-quote-file-name dir-to-save)
                                          (shell-quote-file-name buffer-file-name)
                                          (shell-quote-file-name backup-file))))
          (shell-command command-to-execute t))))))


(defun get-backup-file-path (dir-to-save file-name)
  "Returns path to save a backup file:

DIR-TO-SAVE/<date>_<FILE-NAME>

DIR-TO-SAVE is the destination for the file FILE-NAME"
  (concat dir-to-save
          (format-time-string "%Y-%m-%dT%T")
          "_"
          (file-name-nondirectory file-name)))


(defun file-needs-to-be-saved-p (dir-to-save)
  "Returns T if a new file can be added to the DIR-TO-SAVE.

A file can be added if there are no files in DIR-TO-SAVE-FILE
newer than LOCAL-HISTORY-SAVE-MIN-INTERVAL."
  (= 0 (recent-files-in-dir dir-to-save)))


(defun recent-files-in-dir (dir)
  "Returns number of recent (newer than LOCAL-HISTORY-SAVE-MIN-INTERVAL) files in DIR."
  (length (shell-command-to-string
           (format "find %s -type f -newermt '-%d seconds' 2>/dev/null"
                   dir
                   local-history-save-min-interval))))


(defun shell-quote-file-name (filename)
  (shell-quote-argument (expand-file-name filename)))


(defun save-file-history-asyn (&optional args)
  (make-thread #'save-file-history "save-file-history"))


(add-hook 'after-save-hook 'save-file-history-asyn)

;;; local-history.el ends here
