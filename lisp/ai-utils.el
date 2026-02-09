;;; ai-utils.el --- Helper functions for GPTel and AI -*- lexical-binding: t -*-

;;; Commentary:
;; Provides helpers for:
;; - `my/gptel-insert-merge-style`: Smerge-style code insertions
;; - `my/gptel-apply-udiff`: Apply unified diffs from LLM responses
;; - `my/gptel-show-context`: Show current gptel context

(require 'gptel)

;; Просмотр контекста
(defun my/gptel-show-context ()
  "Show current gptel context buffers/files in echo area."
  (interactive)
  (if gptel-context--alist
      (message "Context: %s"
               (mapconcat
                (lambda (item)
                  (if (bufferp item)
                      (buffer-name item)
                    (if (stringp item)
                        item
                      "unknown")))
                (mapcar #'car gptel-context--alist)
                ", "))
    (message "Context is empty.")))

(provide 'ai-utils)
;;; ai-utils.el ends here
