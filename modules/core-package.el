;;; core-package.el --- Package management initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This module initializes package.el, adds MELPA archives,
;; and bootstraps use-package and Quelpa.

;;; Code:

(require 'package)

;; 1. Добавляем MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Важно для встроенных пакетов (Emacs 29+)
(setq package-install-upgrade-built-in t)

(package-initialize)

;; 2. Обновляем список пакетов при первом запуске
(unless package-archive-contents
  (package-refresh-contents))

;; 3. Установка use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Автоматически скачивать пакеты

;; 4. Установка Quelpa (для сборки пакетов прямо с GitHub/GitLab)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'quelpa-use-package)
  (package-install 'quelpa-use-package))

(require 'quelpa-use-package)

;; 5. Настройка Native Compilation
;; defvar нужен, чтобы подавить warning "assignment to free variable",
;; так как эта переменная определена в C-коде или загружается позже.
(defvar native-comp-async-report-warnings-errors)
(setq native-comp-async-report-warnings-errors nil)

(provide 'core-package)
;;; core-package.el ends here
