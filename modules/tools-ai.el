(provide 'tools-ai)

;; 1. Загружаем твои личные библиотеки из lisp/
;; Так как папка lisp/ уже добавлена в load-path в init.el,
;; require найдет их автоматически.
(require 'llm-diff)             ; Твой скрипт для git diff
(require 'ai-utils)             ; Твои функции insert-here и context
(require 'llm-tool-collection)  ; Твои тулы (убедись, что файл llm-tool-collection.el лежит в lisp/)

;; Хелпер для ключа (можно перенести в ai-utils, но можно и тут)
(defun my-get-openrouter-api-key () (getenv "OPENROUTER_API_KEY"))

;; 2. Настройка GPTel
(use-package gptel
  :ensure t
  :config
  ;; Настройки OpenRouter [cite: 8]
  (setq gptel-model 'google/gemini-3-flash-preview
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (my-get-openrouter-api-key) ;; Используем хелпер или getenv напрямую
          :models '("google/gemini-3-flash-preview")
          :request-params '(:reasoning (:enabled :json-false))))

  ;; Подключаем тулы из твоей коллекции [cite: 8]
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all)))

;; Пресет для ревью [cite: 14]
(gptel-make-preset 'review
  :system "Carefully review the Pull-Request from the buffer.
Provide a top-level overview of the changes and highlight anything that may require my human touch.")

;; 3. Настройка Aidermacs [cite: 15]
(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-backend 'vterm
        aidermacs-vterm-multiline-newline-key "S-<return>"
        aidermacs-editor-model "openrouter/deepseek/deepseek-v3.2"
        aidermacs-architect-model "openrouter/deepseek/deepseek-v3.2")
  (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openrouter/deepseek/deepseek-v3.2"))

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
    map)
  "My key customizations for AI.")

(keymap-global-set "C-x C-." my-gptel-map)
