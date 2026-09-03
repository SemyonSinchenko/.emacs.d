;;; desktop-edit.el --- IDE-like editing for config and data formats -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing support for the config/data formats the IDE knows and the
;; Desktop misses:
;; - JSON/JS/TS indent with 2 spaces (shared with apheleia's prettier
;;   invocation, same as modules/lang-lsp.el),
;; - Markdown, YAML (+ yaml-pro), TOML modes (as in lang-prog.el),
;; - CSV/TSV with rainbow colors and size-limited auto alignment,
;; - apheleia formatting: automatic in JSON/YAML/TOML buffers,
;;   manual with C-x x f anywhere.

;;; Code:

(require 'desktop-config-defs)

;; --- Indent: JSON / JS / TS (2 spaces) ------------------------------

;; `.json' opens in `js-json-mode', which inherits `js-indent-level'
;; 4 and `indent-tabs-mode' t by default.  Most JSON in the wild uses
;; 2 spaces.  The ts-mode offsets already default to 2, set explicitly
;; for consistency.  Apheleia reads the same variables when building
;; prettier arguments (`apheleia-formatters-indent'), so this covers
;; formatting as well.
(setq js-indent-level my-desktop-json-indent-level
      json-ts-mode-indent-offset my-desktop-json-indent-level
      typescript-ts-mode-indent-offset my-desktop-json-indent-level)
(setq-default indent-tabs-mode nil)

;; --- Markdown / YAML / TOML (as in modules/lang-prog.el) -----------

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command my-desktop-markdown-command))

(use-package yaml-mode
  :ensure t)

(use-package yaml-pro
  :ensure t
  :hook (yaml-mode . yaml-pro-mode))

;; TOML: Emacs 29 already maps .toml to the built-in `conf-toml-mode'.
;; Prefer `toml-ts-mode' only when its tree-sitter grammar is actually
;; available (otherwise it starts with warnings and no highlighting);
;; the IDE gets this automatically via treesit-auto.
(when (and (fboundp 'toml-ts-mode)
           (fboundp 'treesit-ready-p)
           (treesit-ready-p 'toml t))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

;; --- CSV / TSV + rainbow-csv ---------------------------------------

(defun my-desktop--csv-buffer-size ()
  "Return the size of the current buffer in characters."
  (- (point-max) (point-min)))

(defun my-desktop--csv-setup ()
  "Guess the separator and auto-align small CSV/TSV buffers."
  (setq-local truncate-lines t)
  (ignore-errors
    (csv-guess-set-separator))
  (when (< (my-desktop--csv-buffer-size)
           my-desktop-csv-align-size-limit)
    (csv-align-mode 1)))

(defun my-desktop--rainbow-csv-maybe ()
  "Enable `rainbow-csv-mode' in small CSV/TSV buffers."
  (when (< (my-desktop--csv-buffer-size)
           my-desktop-rainbow-csv-size-limit)
    (rainbow-csv-mode 1)))

(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . tsv-mode))
  :custom
  (csv-align-max-width my-desktop-csv-align-width)
  :bind (:map csv-mode-map
              ("C-c C-m" . csv-align-mode)      ; toggle column alignment
              ("C-c C-g" . csv-guess-set-separator))
  :hook ((csv-mode tsv-mode) . my-desktop--csv-setup))

(use-package rainbow-csv
  :ensure t
  :after csv-mode
  :hook ((csv-mode tsv-mode) . my-desktop--rainbow-csv-maybe))

;; --- Apheleia (formatting), wiring as in modules/lang-lsp.el -------

(when my-desktop-enable-apheleia
  (use-package apheleia
    :ensure t
    :config
    (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
    (put 'apheleia-mode-alist 'safe-local-variable #'listp)
    ;; Format JSON/YAML/TOML config buffers on save; anywhere else
    ;; with C-x x f.  Markdown stays formatter-less by default (same
    ;; as the IDE and upstream apheleia).
    (dolist (hook '(js-json-mode-hook json-ts-mode-hook
                    yaml-mode-hook yaml-ts-mode-hook
                    conf-toml-mode-hook toml-ts-mode-hook))
      (add-hook hook #'apheleia-mode))
    (keymap-global-set "C-x x f" #'apheleia-format-buffer)))

(provide 'desktop-edit)
;;; desktop-edit.el ends here
