;;; tools-ai.el --- AI tools configuration (GPTel, Aider) -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures AI coding assistants:
;; - GPTel: configured for OpenRouter (Gemini, etc.) with custom tools.
;; - Aidermacs: integration with Aider CLI via vterm.

;;; Code:

;; 1. Загружаем твои личные библиотеки из lisp/
;; (Flycheck может ругаться, что не видит их, так как не знает про init.el)
(require 'llm-diff)             ; [cite_start]Твой скрипт для git diff [cite: 5]
(require 'ai-utils)             ; [cite_start]Твои функции insert-here и context [cite: 9]
(require 'llm-tool-collection)  ; [cite_start]Твои тулы [cite: 8]

;; Хелпер для ключа
(defun my-get-openrouter-api-key ()
  "Return OpenRouter API key from environment."
  (getenv "OPENROUTER_API_KEY"))

;; 2. Настройка GPTel
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'google/gemini-3-flash-preview
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (my-get-openrouter-api-key)
          :models '(
		    "minimax/minimax-m2.1"
		    "z-ai/glm-4.7")
          :request-params '(:reasoning (:enabled :json-false))))

  ;; [cite_start]Подключаем тулы из твоей коллекции [cite: 8]
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all)))

;; [cite_start]Пресет для ревью [cite: 14]
(gptel-make-preset 'review
  :system "Carefully review the Pull-Request from the buffer.
Provide a top-level overview of the changes and highlight anything that may require my human touch.")

;; 3. Aider
(require 'tools-aider-custom)

;; 4. Клавиши (Keymap)
;; Использует функции из lisp/ai-utils.el и lisp/llm-diff.el
(defvar my-gptel-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "g" #'gptel)
    (keymap-set map "s" #'gptel-send)
    (keymap-set map "r" #'gptel-rewrite)
    (keymap-set map "a" #'gptel-add)
    (keymap-set map "f" #'gptel-add-file)
    (keymap-set map "m" #'gptel-menu)
    (keymap-set map "l" #'my/gptel-show-context)      ; из ai-utils.el
    (keymap-set map "i" #'my/gptel-insert-here)       ; из ai-utils.el
    (keymap-set map "x" #'gptel-context-remove-all)
    (keymap-set map "q" #'gptel-abort)
    (keymap-set map "d" #'my/llm-smart-diff)          ; из llm-diff.el
    (keymap-set map "a" #'my/aider-run)               ; aider
    map)
  "My key customizations for AI.")

(keymap-global-set "C-x C-." my-gptel-map)


(provide 'tools-ai)
;;; tools-ai.el ends here
