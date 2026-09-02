;;; desktop-completion.el --- Minibuffer stack for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Same Vertico/Consult stack as the IDE config (minus code
;; completion, which belongs to the IDE, not the desktop).

;;; Code:

(require 'desktop-config-defs)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("C-s" . vertico-next)
              ("C-r" . vertico-previous)))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ;; C-x b is the workspace-scoped buffer switcher (desktop-keys);
   ;; C-x B lists the buffers of ALL workspaces.
   ("C-x B" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-s r" . consult-ripgrep)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(provide 'desktop-completion)
;;; desktop-completion.el ends here
