;;; tools-aider-custom.el --- Custom CLI wrapper for Aider via Vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; A lightweight wrapper around 'aider' CLI.
;; Simplified Logic: ALWAYS runs in --architect mode.
;; - --model is set to the Architect model.
;; - --editor-model is set to the Editor model.
;; - Window logic: Smart split (like Magit/display-buffer).

;;; Code:

(require 'vterm)
(require 'project)

(defgroup my-aider nil
  "Custom Aider configuration."
  :group 'tools)

;; --- 1. Настройки моделей ---

(defcustom my-aider-program "aider"
  "Path to the aider executable."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-architect-model "z-ai/glm-4.7"
  "The 'Thinking' model. In architect mode, this is passed as --model."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-editor-model "minimax/minimax-m2.1"
  "The 'Coding' model. Passed as --editor-model."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-weak-model "minimax/minimax-m2.1"
  "Weak model (--weak-model) for simple tasks."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-args
  '("--no-auto-commits"     ;; Изменения остаются unstaged
    "--no-dirty-commits"    ;; Не коммитить "грязные" изменения
    "--no-gui"              ;; Только CLI
    "--show-model-warnings" ;; Показывать стоимость
    "--show-diffs")         ;; Показывать текстовый дифф
  "List of additional arguments to pass to aider."
  :type '(repeat string)
  :group 'my-aider)

;; --- 2. Логика запуска ---

(defun my/aider--format-model (model)
  "Ensures model name starts with openrouter/ if it's not a local one."
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

(defun my/aider-run (&optional restart)
  "Open or create Aider vterm buffer in the project root."
  (interactive "P")
  (let* ((project (project-current nil))
         (root (if project (project-root project) default-directory))
         (buffer-name (format "*aider:%s*" (file-name-nondirectory (directory-file-name root))))
         (buffer (get-buffer buffer-name)))

    ;; Рестарт (убиваем старый, если есть)
    (when (and restart buffer)
      (kill-buffer buffer)
      (setq buffer nil))

    ;; Создаем новый, если нет
    (unless buffer
      (with-current-buffer (vterm buffer-name)
        (setq buffer (current-buffer))
        (vterm-send-string (format "cd %s\n" root))
        (vterm-send-string (concat (my/aider-get-command) "\n"))))

    ;; --- Умное управление окнами ---
    (let ((window (get-buffer-window buffer)))
      (if window
          ;; 1. Если Aider уже виден где-то -> просто прыгаем туда
          (select-window window)
        
        ;; 2. Если не виден
        (if (one-window-p)
            ;; Сценарий А: Окно одно на весь экран -> Делим пополам
            (progn
              (split-window-right)
              (other-window 1)
              (switch-to-buffer buffer))
          
          ;; Сценарий Б: Окон уже несколько -> Используем "соседнее"
          (progn
            (other-window 1)
            (switch-to-buffer buffer)))))))

(provide 'tools-aider-custom)
;;; tools-aider-custom.el ends here
