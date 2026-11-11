;;; desktop-dbx.el --- Databricks workbench (dbx-el) under C-c d -*- lexical-binding: t; -*-

;;; Commentary:
;; dbx-el (https://github.com/SemyonSinchenko/dbx-el): SQL editor with
;; Org-table results, jobs/runs, clusters, warehouses, bundles,
;; pipelines and a Databricks Connect Python REPL.
;;
;; dbx-el is GitHub-only: it is installed with package-vc on first
;; load (the checkout keeps its python/ directory next to the Lisp).
;; A failed install is only warned about.  Nothing is loaded eagerly:
;; the package autoloads `dbx-command-map', which desktop-keys.el binds
;; to C-c d (C-c d d opens the dbx menu, C-c d D runs the doctor).
;; C-c d o opens any job / run / cluster / ... by ID or pasted URL.
;; desktop-dbx-favorites.el adds favorite jobs (C-c d f, * on job
;; pages); it needs dbx-el with the public API of 2026-11 (commit
;; 7421cc4: `dbx-with-context', `dbx-job-at-point', ...).
;;
;; Settings: `my-desktop-dbx-url', `my-desktop-dbx-contexts' (copied to
;; `dbx-contexts' when non-nil), `my-desktop-dbx-mode-line',
;; `my-desktop-dbx-favorites-file-name', `my-desktop-dbx-favorites-runs'.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(defvar dbx-contexts)
(declare-function my-desktop--warn "desktop-init")
(declare-function dbx-mode-line-mode "dbx-context" (&optional arg))

(unless (locate-library "dbx-el")
  (require 'package-vc)
  (condition-case err
      (package-vc-install `(dbx-el :url ,my-desktop-dbx-url))
    (error (my-desktop--warn "dbx-el install failed: %s"
                             (error-message-string err)))))

;; Favorites (desktop-dbx-favorites.el) load with dbx-jobs; until
;; then the commands are autoloads.  An older dbx-el checkout lacks
;; the API they use: warn instead of failing on first use.
(dolist (cmd '(my-dbx-favorites my-dbx-favorite-toggle))
  (autoload cmd "desktop-dbx-favorites" nil t))
(with-eval-after-load 'dbx-jobs
  (if (fboundp 'dbx-job-latest-runs)
      (require 'desktop-dbx-favorites)
    (my-desktop--warn "dbx-el is too old for favorites: %s"
                      "M-x package-vc-upgrade RET dbx-el, then restart")))

(when my-desktop-dbx-contexts
  (with-eval-after-load 'dbx-context
    (setq dbx-contexts my-desktop-dbx-contexts)))

(when (and my-desktop-dbx-mode-line (fboundp 'dbx-mode-line-mode))
  (dbx-mode-line-mode 1))

(provide 'desktop-dbx)
;;; desktop-dbx.el ends here
