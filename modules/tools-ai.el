;;; tools-ai.el --- AI tools configuration (ECA) -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures the ECA coding assistant.

;;; Code:

;; ECA
(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :custom
  (eca-chat-use-side-window nil))

;; Unified AI map
(defvar my-ai-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "e" #'eca)
    (keymap-set map "r" #'eca-rewrite)
    map)
  "My key customizations for AI.")

(keymap-global-set "C-x C-." my-ai-map)
(keymap-global-set "C-x C-x" my-ai-map)

(provide 'tools-ai)
;;; tools-ai.el ends here
