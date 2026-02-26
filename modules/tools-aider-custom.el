;;; tools-aider-custom.el --- Custom CLI wrapper for Aider via Vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; A lightweight wrapper around 'aider' CLI.
;; Simplified Logic: ALWAYS runs in --architect mode.
;; - --model is set to the Architect model.
;; - --editor-model is set to the Editor model.
;; - Window logic: Smart split (like Magit/display-buffer).

;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup my-aider nil
  "Custom Aider configuration."
  :group 'tools)

;; --- 1. Настройки моделей ---

(defcustom my-aider-program "aider"
  "Path to the aider executable."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-architect-model "anthropic/claude-sonnet-4.6"
  "The \='Thinking' model.  In architect mode, this is passed as --model."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-editor-model "anthropic/claude-sonnet-4.6"
  "The \='Coding' model.  Passed as --editor-model."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-weak-model "qwen/qwen3-235b-a22b-2507"
  "Weak model (--weak-model) for simple tasks."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-args
  '("--no-attribute-co-authored-by" ;; No git attribute
    "--no-analytics"                ;; Disable analytics
    "--no-git-commit-verify"        ;; No hooks
    "--dark-mode"                   ;; Enable dark mode for terminal
    "--no-gui"                      ;; Disable TUI
    "--show-model-warnings"         ;; Показывать стоимость
    "--show-diffs")                 ;; Показывать текстовый дифф
  "List of additional arguments to pass to aider."
  :type '(repeat string)
  :group 'my-aider)

;; --- 2. Логика запуска ---

(defun my/aider--format-model (model)
  "Ensures MODEL name start with openrouter/ if it's not a local one."
  (if (or (string-empty-p model)
          (string-prefix-p "openrouter/" model))
      model
    (concat "openrouter/" model)))

(defun my/aider-get-command ()
  "Build the full aider command string.
ALWAYS: aider --architect --model <ARCHITECT> --editor-model <EDITOR> ..."
  (let ((cmd (list my-aider-program)))
    
    ;; 1. Включаем режим архитектора всегда
    (push "--architect" cmd)

    ;; 2. Architect Model (идет как --model)
    (when (and my-aider-architect-model (not (string-empty-p my-aider-architect-model)))
      (push "--model" cmd)
      (push (my/aider--format-model my-aider-architect-model) cmd))
    
    ;; 3. Editor Model
    (when (and my-aider-editor-model (not (string-empty-p my-aider-editor-model)))
      (push "--editor-model" cmd)
      (push (my/aider--format-model my-aider-editor-model) cmd))

    ;; 4. Weak Model
    (when (and my-aider-weak-model (not (string-empty-p my-aider-weak-model)))
      (push "--weak-model" cmd)
      (push (my/aider--format-model my-aider-weak-model) cmd))

    ;; Остальные аргументы
    (setq cmd (append (reverse cmd) my-aider-args))
    
    ;; Собираем в строку
    (mapconcat #'identity cmd " ")))

(defun my/aider-run ()
  "Open Aider vterm buffer in the project root.
Always opens in the right window of a split, or creates a split if needed."
  (interactive)
  (let* ((project (project-current nil))
         (root (if project (project-root project) default-directory))
         (buffer-name (format "*aider:%s*" (file-name-nondirectory (directory-file-name root))))
         (current-window (selected-window)))

    ;; If window is already split (has a right neighbor), use it; otherwise split right
    (if (window-right current-window)
        (select-window (window-right current-window))
      (select-window (split-window-right)))

    ;; Create or switch to the Aider buffer in this window
    (switch-to-buffer (vterm buffer-name))
    (vterm-send-string (format "cd %s\n" root))
    (vterm-send-string (concat (my/aider-get-command) "\n"))))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "S-<return>")
	      (lambda ()
		(interactive)
		(vterm-insert "\n"))))

(provide 'tools-aider-custom)
;;; tools-aider-custom.el ends here
