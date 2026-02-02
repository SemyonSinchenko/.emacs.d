;;; ai-utils.el --- Helper functions for GPTel and AI -*- lexical-binding: t -*-

(require 'gptel)

(defun my/gptel-insert-merge-style (instruction)
  "Generate code and insert it as a merge conflict for review with a detailed prompt."
  (interactive "sInstruction: ")
  (let* ((original-buffer (current-buffer))
         (file-name (buffer-name))
         (major-mode-name (symbol-name major-mode))
         (context-before (buffer-substring (max (point-min) (- (point) 700)) (point)))
         (context-after (buffer-substring (point) (min (point-max) (+ (point) 700))))
         (start-marker (copy-marker (point) t)))

    (message "GPTel: Expert AI is thinking...")

    (gptel-request
     ;; Сообщение пользователя с явными границами
     (format "FILE: %s
LOCAL CONTEXT BEFORE:
>>>
%s
<<<
LOCAL CONTEXT AFTER:
>>>
%s
<<<
INSTRUCTION: %s"
             file-name context-before context-after instruction)
     
     :system (format "You are a Senior Software Engineer specializing in %s.
Your goal is to generate a surgical code insertion.

CRITICAL RULES:
1. OUTPUT FORMAT: Provide ONLY the raw source code.
2. NO MARKDOWN: Do not use triple backticks (```) or any conversational fillers.
3. NO REPETITION: Do not include code that already exists in the 'CONTEXT BEFORE' or 'CONTEXT AFTER' sections.
4. ATOMICITY: Generate only what is missing at the exact point of the <CURSOR> (which is between BEFORE and AFTER sections).
5. STYLE: Match the indentation (spaces/tabs), naming conventions (camelCase/snake_case), and architectural patterns of the provided context.
6. TASK: If the instruction is 'write docstring', generate ONLY the comment block for the entity immediately following the cursor." major-mode-name)
     
     :stream t
     :callback
     (lambda (response info)
       (cond
        ((stringp response)
         (with-current-buffer (plist-get info :buffer)
           (save-excursion
             (goto-char start-marker)
             (insert response))))
        
        ((and (eq response t) (plist-get info :status))
         (with-current-buffer (plist-get info :buffer)
           (let ((end-marker (point-marker)))
             ;; Создаем блок конфликта для smerge
             (goto-char end-marker)
             (insert "\n>>>>>>> AI PROPOSAL")
             (goto-char start-marker)
             (insert "<<<<<<< ORIGINAL\n=======\n")
             
             (smerge-mode 1)
             ;; Автоматически переходим к следующему конфликту (этому самому)
             (smerge-next)
             (message "Review with C-c ^ (n/p to navigate, o to keep AI, m to keep original)")))))))))

;; Просмотр контекста
(defun my/gptel-show-context ()
  "Show current gptel context buffers in echo area."
  (interactive)
  (if gptel-context--alist
      (message "Context: %s"
               (mapconcat #'buffer-name (mapcar #'car gptel-context--alist) ", "))
    (message "Context is empty.")))

(provide 'ai-utils)
;;; ai-utils.el ends here
