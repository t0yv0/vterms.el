;;; vterms.el --- Managing multiple buffer-associated vterm buffers  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:
;;;
;;; Code:


(require 'project)
(require 'vterm)


(defvar-local vterms--association nil
  "This buffer-local variable stores buffer associations.

If the current buffer is not a Vterm buffer, it may store the
associated Vterm buffer.

If the current buffer is a Vterm buffer, it may store the
directory it was originally opened in.")


;;;###autoload
(defun vterms-toggle ()
  "Toggles between a normal buffer and the associated Vterm buffer."
  (interactive)
  (when (and (not (equal major-mode 'vterm-mode))
             vterms--association
             (not (buffer-live-p vterms--association)))
    (setq vterms--association nil))
  (let ((buf nil)
        (buf-dir nil))
    (cond
     ;; if in Vterm mode already, simply close it
     ((equal major-mode 'vterm-mode)
      (delete-window))
     ;; existing associated Vterm buffer
     (vterms--association
      (switch-to-buffer vterms--association))
     ;; existing project-associated Vterm buffer
     ((let* ((root (vterms--project-root)))
        (when root
          (setq buf (vterms--find-by-dir root)))
        (and root buf))
      (setq vterms--association buf)
      (switch-to-buffer buf))
     ;; existing dir-associated Vterm buffer
     ((let* ((d (vterms--dir)))
        (when d
          (setq buf (vterms--find-by-dir d)))
        (and d buf))
      (setq vterms--association buf)
      (switch-to-buffer buf))
     ;; there exists a *vterm* buffer
     ((get-buffer "*vterm*")
      (setq buf (get-buffer "*vterm*"))
      (setq vterms--association buf)
      (switch-to-buffer buf))
     ;; need a new Vterm buffer
     (t
      (setq buf-dir (or (vterms--project-root)
                                 (vterms--dir)))
      (setq buf (vterms--new buf-dir))
      (setq vterms--association buf)
      (with-current-buffer buf
        (setq vterms--association buf-dir))
      (switch-to-buffer buf)))))


;;;###autoload
(defun vterms-repeat ()
  "Clear the associated Vterm and re-submit the last command to it."
  (interactive)
  (save-window-excursion
    (unless (equal major-mode 'vterm-mode)
      (vterms-toggle))
    (vterm-clear)
    (vterm-send-key "<up>")
    (vterm-send-return)))


;;;###autoload
(defun vterms-cd ()
  "Change the directory of the associated Vterm buffer to the current buffer's directory."
  (interactive)
  (unless (equal major-mode 'vterm-mode)
    (let ((d (vterms--dir)))
      (save-window-excursion
        (vterms-toggle)
        (vterm-clear)
        (vterm-send-string (concat "cd " (shell-quote-argument d)))
        (vterm-send-return)))))


(defun vterms--name-suggestion-for-directory (directory)
  "Suggest a name part for a Vterm buffer based on DIRECTORY."
  (cond ((null directory) "")
        (t (concat "-" (file-name-nondirectory
                        (substring directory 0 (- (length directory) 1)))))))


(defun vterms--new (&optional root)
  "Create a new Vterm buffer in ROOT and return it."
  (let* ((name-suggestion (vterms--name-suggestion-for-directory root))
         (buffer (vterms--fresh-name name-suggestion)))
    (if root
        (let ((default-directory root))
          (vterm buffer))
      (vterm buffer))
    buffer))


(defun vterms--fresh-name (name-suggestion)
  "Compute a fresh buffer name for a Vterm buffer.

Use NAME-SUGGESTION as part of the fresh name to generate.

Avoid conflicts with existing buffers."
  (let ((choice nil)
        (index 0))
    (while (null choice)
      (let ((n (vterms--full-name name-suggestion index)))
        (unless (get-buffer n)
          (setq choice n)))
      (setq index (+ index 1)))
    choice))


(defun vterms--full-name (name-suggestion index)
  "Compute a name candidate at INDEX from NAME-SUGGESTION."
  (if (equal index 0)
      (format "*vterm%s*" name-suggestion)
    (format "*vterm%s-%s*" name-suggestion index)))


(defun vterms--find-by-dir (dir)
  "Find a Vterm buffer created for a given directory."
  (seq-find
   (lambda (b)
     (with-current-buffer b
       (and
        (equal major-mode 'vterm-mode)
        (equal vterms--association dir))))
   (buffer-list)))


(defun vterms--dir ()
  "Retrieve current buffer's file directory if any."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    nil))


(defun vterms--project-root ()
  "Retrieve current project root if any."
  (if (null (project-current))
      nil
    (project-root (project-current))))


(provide 'vterms)
;;; vterms.el ends here
