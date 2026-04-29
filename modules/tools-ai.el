;;; tools-ai.el --- AI tools configuration (Aider, Copilot, ECA) -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures AI coding assistants:
;; - Aider: integration with Aider CLI via eat.
;; - Copilot and ECA helpers.

;;; Code:

;; Aider
(require 'tools-aider-custom)

;; ECA
(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :custom
  (eca-chat-use-side-window nil))

;; Unified AI map (without GPTel)
(defvar my-ai-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'my/aider-menu)
    (keymap-set map "e" #'eca)
    (keymap-set map "r" #'eca-rewrite)
    map)
  "My key customizations for AI.")

(keymap-global-set "C-x C-." my-ai-map)
(keymap-global-set "C-x C-x" my-ai-map)

(provide 'tools-ai)
;;; tools-ai.el ends here
