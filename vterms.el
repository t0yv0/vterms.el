;;; vterms.el --- Managing multiple buffer-associated vterm buffers  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:
;;;
;;; Code:


(require 'project)
(require 'vterm)


;;;###autoload
(defun vterms-switch ()
  "Display a Vterm buffer associated with the current buffer."
  (interactive)
  (cond
   ((vterms--associated-buffer)
    (switch-to-buffer (vterms--associated-buffer)))
   (t
    (error "%s" "No associated vterm buffer"))))


;;;###autoload
(defun vterms-new ()
  "Create a Vterm buffer associated with the current buffer.

The Vterm buffer will start in the directory where the current
buffer's file is located. It will also be named after this
directory."
  (interactive)
  (vterms--new (if buffer-file-name
                  (file-name-directory buffer-file-name)
                 nil)))


;;;###autoload
(defun vterms-new-in-project-root ()
  "Like `vterms-new` but opens the Vterm in the current project's
root directory."
  (interactive)
  (vterms--new (if (null (project-current))
                   nil
                 (project-root (project-current)))))


;;;###autoload
(defun vterms-repeat ()
  "Clear a visible Vterm and re-submit the last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (vterms--visible-buffer)))
      (when buf
        (with-current-buffer buf
          (vterm-clear)
          (vterm-send-key "<up>")
          (vterm-send-return))))))


;;;###autoload
(defun vterms-window ()
  "Select a visible Vterm window."
  (interactive)
  (let ((b (vterms--visible-buffer)))
    (when b
      (switch-to-buffer b))))


(defun vterms--name-suggestion-for-directory (directory)
  "Suggest a name part for a Vterm buffer based on a directory."
  (cond ((null directory) "")
        (t (concat "-" (file-name-nondirectory
                        (substring directory 0 (- (length directory) 1)))))))


(defun vterms--new (root)
  (let* ((name-suggestion (vterms--name-suggestion-for-directory root))
         (buffer (vterms--fresh-name name-suggestion)))
    (vterms--db-register (buffer-name (current-buffer)) buffer root)
    (if root
        (let ((default-directory root))
          (vterm buffer))
      (vterm buffer))
    (switch-to-buffer buffer)
    buffer))


(defun vterms--associated-buffer ()
  (vterms--db-lookup-vterm-buffer-name (buffer-name (current-buffer))))


(defun vterms--visible-buffer ()
  "Find a visible vterm buffer.

This buffer must be associated with one of the windows in the
current frame. Returns nil if none is found."
  (let ((found nil))
    (dolist (it (window-list))
      (if (and (null found)
               (equal 'vterm-mode (with-current-buffer
                                      (window-buffer it)
                                    major-mode)))
          (setq found it)))
    (if found (window-buffer found) nil)))


(defun vterms--fresh-name (name-suggestion)
  "Compute a fresh buffer name for a Vterm buffer.

Avoids conflicts with currently registered buffers."
  (let ((choice nil)
        (index 0))
    (while (null choice)
      (let ((n (vterms--full-name name-suggestion index)))
        (unless (vterms--db-taken n)
          (setq choice n)))
      (setq index (+ index 1)))
    choice))


(defun vterms--full-name (name-suggestion index)
  (if (equal index 0)
      (format "*vterm%s*" name-suggestion)
    (format "*vterm%s-%s*" name-suggestion index)))


(defvar vterms--db
  (list :by-regular-buffer-name (make-hash-table :test 'equal)
        :by-vterm-buffer-name (make-hash-table :test 'equal)
        :by-root (make-hash-table :test 'equal)))


(defun vterms--db-taken (vterm-buffer-name)
  (if (gethash vterm-buffer-name (plist-get vterms--db :by-vterm-buffer-name)) t nil))


(defun vterms--db-register (regular-buffer-name vterm-buffer-name root)
  (let ((entry (list :vterm-buffer vterm-buffer-name
                     :regular-buffer regular-buffer-name
                     :root root)))
    (puthash regular-buffer-name entry (plist-get vterms--db :by-regular-buffer-name))
    (puthash vterm-buffer-name entry (plist-get vterms--db :by-vterm-buffer-name))
    (when root
      (puthash root entry (plist-get vterms--db :by-root)))))


(defun vterms--db-lookup-vterm-buffer-name (regular-buffer-name)
  (plist-get
   (gethash regular-buffer-name
            (plist-get vterms--db :by-regular-buffer-name))
   :vterm-buffer))


(provide 'vterms)
;;; vterms.el ends here
