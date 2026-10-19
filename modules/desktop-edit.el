;;; desktop-edit.el --- IDE-like editing for config and data formats -*- lexical-binding: t; -*-

;;; Commentary:
;; Editing support for the config/data formats the IDE knows and the
;; Desktop misses:
;; - JSON/JS/TS indent with 2 spaces (shared with apheleia's prettier
;;   invocation, same as modules/lang-lsp.el),
;; - Markdown, YAML (+ yaml-pro), TOML modes (as in lang-prog.el),
;; - smerge-mode gets priority over markdown-modern rendering while
;;   merge conflicts are being resolved (see the smerge section),
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

(use-package markdown-modern
  :vc (:url "https://github.com/rjprins/markdown-modern"
       :lisp-dir "lisp"
       :rev :newest)
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-modern-mode))


;; --- smerge-mode vs markdown-modern --------------------------------

;; markdown-modern-mode turns font-lock off and draws the buffer with
;; its own overlays, so `smerge-mode' shows no conflict highlighting
;; and the <<<<<<< / ======= / >>>>>>> markers are hidden by the
;; rendering.  Conflict resolution wins: while `smerge-mode' is on in
;; a markdown-modern buffer, the buffer falls back to a plain
;; font-locked mode, and markdown-modern comes back once the last
;; conflict is resolved (smerge-mode off).

(require 'smerge-mode)

(defvar-local my-desktop--smerge-owns-buffer nil
  "Non-nil when markdown-modern yielded this buffer to `smerge-mode'.")
;; Survive the major-mode switches this mode juggling performs.
(put 'my-desktop--smerge-owns-buffer 'permanent-local t)

(defun my-desktop--smerge-have-conflict-p ()
  "Return non-nil when the buffer has merge conflict markers."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t))))

(defun my-desktop--smerge-fallback-mode ()
  "Switch the buffer to a plain font-locked Markdown mode."
  (if (fboundp 'markdown-mode)
      (markdown-mode)
    (text-mode)))

(defun my-desktop--smerge-priority ()
  "Give `smerge-mode' priority over markdown-modern rendering.
Runs from `smerge-mode-hook'.  In a markdown-modern buffer the
major mode is switched to `markdown-mode' (or `text-mode') so that
smerge conflict highlighting and the C-c ^ bindings work;
markdown-modern comes back when `smerge-mode' turns off."
  (cond
   ((and smerge-mode (derived-mode-p 'markdown-modern-mode))
    ;; Switching the major mode kills buffer-local `smerge-mode',
    ;; so re-enable it after the switch.
    (my-desktop--smerge-fallback-mode)
    (setq my-desktop--smerge-owns-buffer t)
    (smerge-mode 1))
   ((and (not smerge-mode)
         my-desktop--smerge-owns-buffer
         (or (derived-mode-p 'markdown-mode)
             (eq major-mode 'text-mode)))
    (setq my-desktop--smerge-owns-buffer nil)
    (markdown-modern-mode))))

(defun my-desktop--smerge-md-modern-guard ()
  "Keep an active smerge session when markdown-modern re-enters.
Runs from `markdown-modern-mode-hook': a session started in this
buffer is never dropped by re-entering markdown-modern by hand."
  (when my-desktop--smerge-owns-buffer
    (if (my-desktop--smerge-have-conflict-p)
        (progn
          (my-desktop--smerge-fallback-mode)
          (smerge-mode 1))
      (setq my-desktop--smerge-owns-buffer nil))))

(defun my-desktop--smerge-maybe-enable ()
  "Turn on `smerge-mode' when conflict markers are detected."
  (when (my-desktop--smerge-have-conflict-p)
    (smerge-mode 1)))

(add-hook 'smerge-mode-hook #'my-desktop--smerge-priority)
(add-hook 'find-file-hook #'my-desktop--smerge-maybe-enable)
(add-hook 'after-revert-hook #'my-desktop--smerge-maybe-enable)
(add-hook 'markdown-modern-mode-hook #'my-desktop--smerge-md-modern-guard)

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
