;;; ai-utils.el --- Helper functions for GPTel and AI -*- lexical-binding: t -*-

(require 'gptel)

;; Функция вставки кода (та самая, сложная)
(defun my/gptel-insert-here (instruction)
  "Prompts for an instruction and inserts the generated text at the cursor."
  (interactive "sInstruction: ")
  (let* ((original-buffer (current-buffer))
         ;; Очистка имени режима (python-ts-mode -> python)
         (lang (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (already-in-context (and (boundp 'gptel--context-list)
                                  (member original-buffer (gptel-context-buffers))))
         (start-marker (point-marker)))

    ;; 1. Добавляем в контекст
    (unless already-in-context
      (gptel-add))
    
    (message "GPTel: Writing %s..." lang)

    ;; 2. Запрос
    (gptel-request
     instruction
     :system (format "You are a skilled coding assistant embedded in Emacs.
LANGUAGE: %s
TASK: Generate code based on the user's instruction.
CONTEXT: Use the provided buffers to understand the existing code, style, and variables.
FORMAT: Return ONLY the raw code to be inserted.
- NO Markdown code blocks (```).
- NO conversational text or explanations.
- Do NOT repeat existing surrounding code. Just output exactly what should be inserted at the cursor." lang)
     :context (gptel-context-buffers)
     :stream t
     
     ;; 3. Обработка ответа
     :callback
     (lambda (response info)
       (if response
           (with-current-buffer original-buffer
             (save-excursion
               (goto-char start-marker)
               (insert response)
               (set-marker start-marker (point))))
         
         (when (plist-get info :status)
           (message "GPTel: Done.")
           (unless already-in-context
             (with-current-buffer original-buffer
               (gptel-remove)))))))))

;; Просмотр контекста
(defun my/gptel-show-context ()
  "Show current gptel context buffers in echo area."
  (interactive)
  (if gptel-context--alist
      (message "Context: %s"
               (mapconcat #'buffer-name (mapcar #'car gptel-context--alist) ", "))
    (message "Context is empty.")))

(provide 'ai-utils)
