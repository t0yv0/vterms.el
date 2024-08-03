;;; vterms.el --- Managing multiple buffer-associated vterm buffers  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:
;;;
;;; Code:


(require 'project)
(require 'vterm)


(defvar vterms--table
  ;; create a string to string hash table
  (make-hash-table :test 'equal))


(defun vterms-switch ()
  "Display a Vterm buffer associated with the current buffer.

If there is no Vterm buffer associated with the current buffer,
but the current frame is displaying a Vterm buffer, switch to
that buffer instead.

If there is neither an associated Vterm buffer nor a currently
visible Vterm buffer to switch to, and the current buffer is not
a Vterm one, create a new Vterm buffer, associate it with the
current buffer, and switch to it."
  (interactive)
  (cond
   ((vterms--associated-buffer)
    (switch-to-buffer (vterms--associated-buffer)))
   ((vterms--visible-buffer)
    (switch-to-buffer (vterms--visible-buffer)))
   ((not (eq major-mode 'vterm-mode))
    (vterms-new))))


(defun vterms-new ()
  "Create a Vterm buffer associated with the current buffer.

The Vterm buffer will start in the directory where the current
buffer's file is located. It will also be named after this
directory."
  (interactive)
  (vterms--new (if buffer-file-name
                  (file-name-directory buffer-file-name)
                 nil)))


(defun vterms-new-in-project-root ()
  "Like `vterms-new` but opens the Vterm in the current project's
root directory."
  (interactive)
  (vterms--new (if (null (project-current))
                   nil
                 (project-root (project-current)))))


(defun vterms--name-suggestion-for-directory (directory)
  "Suggest a name part for a Vterm buffer based on a directory."
  (cond ((null directory) "")
        (t (concat "-" (file-name-nondirectory
                        (substring directory 0 (- (length directory) 1)))))))


(defun vterms--new (root)
  (let* ((name-suggestion (vterms--name-suggestion-for-directory root))
        (buffer (vterms--fresh-name name-suggestion)))
    (puthash (buffer-name (current-buffer)) buffer vterms--table)
    (if root
        (let ((default-directory root))
          (vterm buffer))
      (vterm buffer))
    (switch-to-buffer buffer)
    buffer))


(defun vterms--associated-buffer ()
  (gethash (buffer-name (current-buffer)) vterms--table))


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
  (let ((taken (make-hash-table :test 'equal))
        (choice nil)
        (index 0))
    (maphash (lambda (key value) t (puthash value key taken)) vterms--table)
    (while (null choice)
      (let ((n (vterms--full-name name-suggestion index)))
        (unless (gethash n taken)
          (setq choice n)))
      (setq index (+ index 1)))
    choice))


(defun vterms--full-name (name-suggestion index)
  (if (equal index 0)
      (format "*vterm%s*" name-suggestion)
    (format "*vterm%s-%s*" name-suggestion index)))


(provide 'vterms)
;;; vterms.el ends here
