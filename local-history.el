;;; -*- lexical-binding: t; -*-
;;; local-history --- history of all edited files

;; Author: Alexander Akhmetov <a@alx.cx>
;; Version: 0.1.0

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


(defvar local-history-path (concat user-emacs-directory "local-history-files")
  "Default place to keep backup files.")
(defvar local-history-file-size-limit 10485760
  "Maximum file size (in bytes) that should be copied on every save.")
(defvar local-history-save-min-interval 5
  "Minimal interval (in seconds) to save backup files.")


(defun local-history--save-file-history ()
  "Save current buffer to local history folder (LOCAL-HISTORY-PATH)."
  (let* ((dir-to-save (backup-dir-for-current-buffer))
         (backup-file-name (get-backup-file-path dir-to-save buffer-file-name)))
    (when (file-needs-to-be-saved-p dir-to-save)
      (local-history--backup-file dir-to-save buffer-file-name backup-file-name))))


(defun local-history--backup-file (dir-to-save file-name backup-file-name)
  "Copy FILE-NAME to DIR-TO-SAVE with BACKUP-FILE-NAME name."
  (make-directory dir-to-save :parents)
  (copy-file file-name backup-file-name :ok-is-already-exists))


(defun backup-dir-for-current-buffer ()
  "Get a full path to the current buffer's backup directory."
  (concat local-history-path
          (file-name-as-directory buffer-file-name)))


(defun local-history--history-files ()
  "Get latest history files for the buffer."
  (when (buffer-file-name)
    (sort (seq-subseq (directory-files-and-attributes (backup-dir-for-current-buffer) 'full) 2)
          #'(lambda (x y) (if (time-less-p (nth 6 x) (nth 6 y)) nil t)))))


(defun get-backup-file-path (dir-to-save file-name)
  "Return path to save a backup file:

    DIR-TO-SAVE/<date>_<FILE-NAME>

    DIR-TO-SAVE is the destination for the file FILE-NAME"
  (concat dir-to-save
          (format-time-string "%Y-%m-%dT%T")
          "_"
          (file-name-nondirectory file-name)))


(defun file-needs-to-be-saved-p (dir-to-save)
  "Return T if a new file can be added to the DIR-TO-SAVE.

    A file can be added if there are no files in DIR-TO-SAVE-FILE
    newer than LOCAL-HISTORY-SAVE-MIN-INTERVAL.

    AND file size is less or equals the LOCAL-HISTORY-FILE-SIZE-LIMIT."
  (and
   (= 0 (local-history--recent-files-in-dir dir-to-save))
   (<= (buffer-size) local-history-file-size-limit)))


(defun local-history--recent-files-in-dir (dir)
  "Return number of recent (newer than LOCAL-HISTORY-SAVE-MIN-INTERVAL) files in DIR."
  (let ((command (format "find %s -type f -newermt '-%d seconds' 2>/dev/null"
                         dir
                         local-history-save-min-interval)))
    (length (shell-command-to-string command))))


(defun local-history--save-file-history-asyn ()
  "Start a thread to save current buffer as a file asyncronously."
  (when (buffer-file-name)
    (make-thread #'local-history--save-file-history
                 "local-history--save-file-history")))


(defun local-history--set-hook ()
  "Set a hook to save files when the mode is enabled."
  (if local-history-mode
      (add-on-save-hook)
    (remove-on-save-hook)))


(defun add-on-save-hook ()
  "Add `after-save-hook` to save file history."
  (add-hook 'after-save-hook 'local-history--save-file-history-asyn))

(defun remove-on-save-hook ()
  "Remove `after-save-hook`."
  (remove-hook 'after-save-hook 'local-history--save-file-history-asyn))


(define-minor-mode local-history-mode
  "Toggle local history mode.
The command enabled the local hostory mode.

When local-history is enabled, every time when you save a file,
Emacs creates a copy in LOCAL-HISTORY-PATH, so you have history of all edited files."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  "local-history"
  :group 'local-history)


(define-globalized-minor-mode
  global-local-history-mode
  local-history-mode
  (lambda () (local-history-mode t)))


;;;###autoload
(add-hook 'local-history-mode-hook #'local-history--set-hook)


(provide 'local-history)
;;; local-history.el ends here
