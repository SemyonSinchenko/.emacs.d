;;; core-package.el --- Package management initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This module initializes package.el, adds MELPA archives,
;; and bootstraps use-package.
;;
;; elpa/ is shared with the Emacs Desktop (desktop-init.el), which
;; installs many more packages.  Only `my-ide-packages' and their
;; dependencies are activated here; startup activation is disabled in
;; early-init.el.  A new IDE package must be added to `my-ide-packages'.

;;; Code:

(require 'package)

;; 1. Добавляем MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Важно для встроенных пакетов (Emacs 29+)
(setq package-install-upgrade-built-in t)

(setq package-archives
      '(("gnu"      . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
        ("melpa"    . "https://melpa.org/packages/")
        ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

(setq package-archive-priorities
      '(("gnu"      . 10)
        ("nongnu"   . 10)
        ("melpa"    . 5)
        ("jcs-elpa" . 0)))

;; Пакеты IDE; зависимости добавляются автоматически.
(defvar my-ide-packages
  '(;; core-completion
    vertico marginalia orderless consult embark embark-consult
    yasnippet yasnippet-snippets yasnippet-capf corfu kind-icon cape
    ;; core-ui / core-keys
    catppuccin-theme which-key ace-window xclip
    ;; tools-dired / tools-ai
    exec-path-from-shell dired-gitignore magit ghostel rg projectile
    wakatime-mode visual-regexp-steroids eca
    ;; lang-lsp / lang-prog (eldoc, python: newer than built-in)
    treesit-auto apheleia eldoc python python-mode scala-mode jarchive
    sbt-mode rustic markdown-mode yaml-mode yaml-pro protobuf-mode)
  "Packages the IDE config activates from the shared elpa/.")

(defun my-ide--package-closure (names)
  "Return NAMES plus all their installed dependencies."
  (let (seen)
    (while names
      (let ((name (pop names)))
        (unless (memq name seen)
          (push name seen)
          (when-let* ((desc (cadr (assq name package-alist))))
            (dolist (req (package-desc-reqs desc))
              (push (car req) names))))))
    seen))

(package-initialize t)                  ; descriptors only, no activation
(setq package-load-list
      (mapcar (lambda (name) (list name t))
              (my-ide--package-closure my-ide-packages)))
(package-activate-all)

;; 2. Обновляем список пакетов при первом запуске
(unless package-archive-contents
  (package-refresh-contents))

;; 3. Установка use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Автоматически скачивать пакеты

;; 4. Настройка Native Compilation
;; defvar нужен, чтобы подавить warning "assignment to free variable",
;; так как эта переменная определена в C-коде или загружается позже.
(defvar native-comp-async-report-warnings-errors)
(setq native-comp-async-report-warnings-errors nil)

(provide 'core-package)
;;; core-package.el ends here
