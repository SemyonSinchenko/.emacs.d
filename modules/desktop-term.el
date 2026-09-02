;;; desktop-term.el --- Terminals (ghostel) for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; ghostel terminals opened inside the current workspace.  If the
;; native module is missing, run `M-x ghostel-download-module' once.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

;; Declared special so the byte compiler (and the interpreter) treat
;; the let-binding in `my-term-new' as a dynamic one -- it let-binds
;; this ghostel defcustom per invocation.
(defvar ghostel-buffer-name)

(use-package ghostel
  :ensure t
  :commands (ghostel ghostel-project ghostel-download-module
                     ghostel-other consult-ghostel)
  :custom
  (ghostel-module-auto-install nil))

(defun my-term-new (&optional dir)
  "Open a NEW ghostel terminal in the current workspace.
DIR defaults to `my-desktop-term-default-dir', then ~.  Every
call creates a fresh terminal -- it never switches to an existing
one.  Terminals are named after their directory, so same-directory
terminals group as instances: *ghostel:dir*, *ghostel:dir*<2>, ..."
  (interactive)
  (unless (fboundp 'ghostel)
    (user-error "ghostel is disabled or not installed"))
  ;; Load ghostel BEFORE let-binding its defcustom: loading it while
  ;; the binding is active would run ghostel's defcustom inside the
  ;; let and error with "Defining as dynamic an already lexical var".
  (require 'ghostel)
  (let* ((dir (expand-file-name
               (or dir
                   my-desktop-term-default-dir
                   default-directory
                   "~")))
         (name (format "*ghostel:%s*"
                       (file-name-nondirectory
                        (directory-file-name dir)))))
    (let ((default-directory dir)
          (ghostel-buffer-name name))
      ;; Non-numeric prefix arg = ghostel creates the next instance
      ;; instead of switching to the existing one.
      (ghostel '(4)))
    ;; Belt and braces: the terminal is displayed by ghostel through
    ;; display-buffer, which perspective hooks to associate buffers
    ;; -- pin it to the current workspace explicitly anyway.
    (when (bound-and-true-p persp-mode)
      (persp-add-buffer (window-buffer (selected-window))))))

(defun my-term-project ()
  "Open a terminal in a new tab at a chosen project root."
  (interactive)
  (require 'project)
  (let ((dir (project-prompt-project-dir)))
    (my-term-new dir)))

(when my-desktop-enable-popper
  (use-package popper
    :ensure t
    :config
    (setq popper-reference-buffers
          '("\\*Warnings\\*" "\\*sync:" help-mode))
    ;; Group transient popups by the current workspace.
    (setq popper-group-function
          (lambda ()
            (if (bound-and-true-p persp-mode)
                (persp-current-name)
              "main")))
    (popper-mode 1)))

(provide 'desktop-term)
;;; desktop-term.el ends here
