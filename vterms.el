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
  "Select a Vterm buffer associated with the current buffer."
  (interactive)
  (unless (vterms--try-switch)
    (error "%s" "No associated vterm buffer")))


;;;###autoload
(defun vterms-switch-or-new (arg)
  "Try `vterms-switch' falling back on `vterms-new'.

If prefix ARG is given, behave like `vterms-new'."
  (interactive "P")
  (unless (or arg
              (vterms--try-switch)
              (vterms--try-switch-to-root (vterms--dir)))
    (vterms-new)))


;;;###autoload
(defun vterms-switch-or-new-in-project-root (arg)
  "Try `vterms-switch' falling back on `vterms-new-in-project-root'.

If prefix ARG is given, behave like `vterms-new-in-project-root'."
  (interactive "P")
  (unless (or arg
              (vterms--try-switch)
              (vterms--try-switch-to-root (vterms--project-root))
    (vterms-new-in-project-root))))


(defun vterms--try-switch ()
  "Implements `vterms-switch` logic returning a success code."
  (cond
   ((equal major-mode 'vterm-mode)
    (error "%s" "vterms-switch error: already in vterm-mode"))
   ((vterms--associated-buffer)
    (switch-to-buffer (vterms--associated-buffer))
    t)
   (t nil)))


(defun vterms--try-switch-to-root (root)
  "Switch to a buffer associated via ROOT and return a success code."
  (if root
      (let ((b (vterms--db-lookup-vterm-buffer-name-by-root root)))
        (cond
         (b
          (switch-to-buffer b)
          t)
         (t
          nil)))
    nil))


;;;###autoload
(defun vterms-new ()
  "Create a Vterm buffer associated with the current buffer.

The Vterm buffer will start in the directory where the current
buffer's file is located. It will also be named after this
directory."
  (interactive)
  (vterms--new (vterms--dir)))


(defun vterms--dir ()
  "Retrieve current buffer's file directory if any."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    nil))


;;;###autoload
(defun vterms-new-in-project-root ()
  "Create a Vterm buffer associated with the current buffer.

The Vterm buffer will start in the current project's root
directory."
  (interactive)
  (vterms--new (vterms--project-root)))


(defun vterms--project-root ()
  "Retrieve current project root if any."
  (if (null (project-current))
      nil
    (project-root (project-current))))


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
  "Suggest a name part for a Vterm buffer based on DIRECTORY."
  (cond ((null directory) "")
        (t (concat "-" (file-name-nondirectory
                        (substring directory 0 (- (length directory) 1)))))))


(defun vterms--new (root)
  "Create a new Vterm buffer in ROOT, associate and open it."
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
  "Find the Vterm buffer associated with the current buffer, if any."
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

Use NAME-SUGGESTION as part of the fresh name to generate.

Avoid conflicts with currently registered buffers."
  (let ((choice nil)
        (index 0))
    (while (null choice)
      (let ((n (vterms--full-name name-suggestion index)))
        (unless (vterms--db-taken n)
          (setq choice n)))
      (setq index (+ index 1)))
    choice))


(defun vterms--full-name (name-suggestion index)
  "Compute a name candidate at INDEX from NAME-SUGGESTION."
  (if (equal index 0)
      (format "*vterm%s*" name-suggestion)
    (format "*vterm%s-%s*" name-suggestion index)))


(defvar vterms--db
  (list :by-regular-buffer-name (make-hash-table :test 'equal)
        :by-vterm-buffer-name (make-hash-table :test 'equal)
        :by-root (make-hash-table :test 'equal)))


(defun vterms--db-active-entry (entry)
  "Return ENTRY if it is non-nil and refers to real buffers.

Otherwise returns nil."
  (and entry
       (get-buffer (plist-get entry :vterm-buffer))
       (get-buffer (plist-get entry :regular-buffer))
       entry))


(defun vterms--db-lookup (key-type key &optional field)
  "Looks up entries from the db by KEY-TYPE and KEY."
  (let ((e (gethash key (plist-get vterms--db key-type))))
    (when (vterms--db-active-entry e)
      (if field
          (plist-get e field)
        e))))


(defun vterms--db-taken (vterm-buf-name)
  "Check if VTERM-BUF-NAME is already taken."
  (if (vterms--db-lookup :by-vterm-buffer-name vterm-buf-name) t nil))


(defun vterms--db-register (regular-buffer-name vterm-buf-name root)
  "Register a new association from REGULAR-BUFFER-NAME to VTERM-BUF-NAME.

ROOT, if non-nil, is the starting directory of the Vterm buffer."
  (let ((entry (list :vterm-buffer vterm-buf-name
                     :regular-buffer regular-buffer-name
                     :root root)))
    (puthash regular-buffer-name entry (plist-get vterms--db :by-regular-buffer-name))
    (puthash vterm-buf-name entry (plist-get vterms--db :by-vterm-buffer-name))
    (when root
      (puthash root entry (plist-get vterms--db :by-root)))))


(defun vterms--db-lookup-vterm-buffer-name (regular-buffer-name)
  "Find the Vterm buffer name associated with the REGULAR-BUFFER-NAME."
  (vterms--db-lookup :by-regular-buffer-name regular-buffer-name :vterm-buffer))


(defun vterms--db-lookup-vterm-buffer-name-by-root (root)
  "Find the Vterm buffer name associated with the ROOT."
  (vterms--db-lookup :by-root root :vterm-buffer))


(defun vterms--db-active-vterm-buffer-names ()
  "List Vterm buffer names that are associated to a regular buffer."
  (let ((result (list)))
    (maphash (lambda (key entry)
               (when (vterms--db-active-entry entry)
                 (setq result (cons key result))))
             (plist-get vterms--db :by-vterm-buffer-name))
    (reverse result)))


(provide 'vterms)
;;; vterms.el ends here
