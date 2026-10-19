;;; desktop-term.el --- Terminals (ghostel) for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; ghostel terminals opened inside the current workspace.  If the
;; native module is missing, run `M-x ghostel-download-module' once.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

;; Declared special so the byte compiler (and the interpreter) treat
;; the references in `my-term-new' as dynamic ones -- it let-binds
;; `ghostel-buffer-name' and buffer-locally clears
;; `ghostel-buffer-name-function' per invocation.
(defvar ghostel-buffer-name)
(defvar ghostel-buffer-name-function)

(declare-function persp-current-name "perspective")

(use-package ghostel
  :ensure t
  :commands (ghostel ghostel-project ghostel-download-module
                     ghostel-other consult-ghostel)
  :custom
  (ghostel-module-auto-install nil))

(defun my-term--next-id (ws)
  "Return the next terminal number for workspace WS.
One plus the highest N among live buffers named \"WS-terminal-N\",
so numbers grow monotonically per workspace and stay unique while
any of its terminals is alive."
  (let ((re (format "\\`%s-terminal-\\([0-9]+\\)\\'"
                    (regexp-quote ws)))
        (top 0))
    (dolist (buf (buffer-list))
      (let ((name (and (buffer-live-p buf) (buffer-name buf))))
        (when (and name (string-match re name))
          (setq top (max top (string-to-number
                              (match-string 1 name)))))))
    (1+ top)))

(defun my-term-new (&optional dir)
  "Open a NEW ghostel terminal in the current workspace.
DIR defaults to `my-desktop-term-default-dir', then ~.  Every
call creates a fresh terminal -- it never switches to an existing
one.  Terminals are numbered per workspace after the perspective
name: spark-terminal-1, spark-terminal-2, ..."
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
         (ws (if (bound-and-true-p persp-mode)
                 (or (persp-current-name) "main")
               "main"))
         (name (format "%s-terminal-%d" ws (my-term--next-id ws))))
    (let ((default-directory dir)
          (ghostel-buffer-name name))
      ;; Non-numeric prefix arg = ghostel creates a fresh buffer
      ;; instead of switching to an existing one.
      (ghostel '(4)))
    ;; Keep the assigned name: ghostel renames managed buffers on
    ;; title (OSC 2) and cd (OSC 7) reports through its default
    ;; title-based naming function.
    (setq-local ghostel-buffer-name-function nil)
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
