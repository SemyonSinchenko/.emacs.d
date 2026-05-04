;;; core-completion.el --- Minibuffer and Code Completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures the completion ecosystem:
;; - Minibuffer: Vertico, Consult, Marginalia, Orderless, Embark.
;; - In-buffer: Corfu, Cape, Yasnippet.

;;; Code:

(declare-function eglot-completion-at-point "eglot")

;; --- Minibuffer (Vertico/Consult) ---

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("C-s" . vertico-next)
              ("C-r" . vertico-previous))
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  ;; Строка с хуком удалена, так как consult-preview-at-point-mode больше не существует.
  ;; Превью работает "из коробки".
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-s r" . consult-ripgrep)
   ("M-s f" . consult-find)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;; --- Code Completion (Corfu + Cape + Yasnippet) ---

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yasnippet-capf
  :ensure t
  :after yasnippet)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (defun my/eglot-capf ()
    "Combine Eglot and Yasnippet for completion-at-point."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf)))))

(provide 'core-completion)
;;; core-completion.el ends here
