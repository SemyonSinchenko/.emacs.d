;;; lang-prog.el --- Programming language specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures specific programming languages (Python, Java, Scala, Rust, etc.).
;; It handles mode hooks, LSP server connections (via Eglot), and formatters.

;;; Code:

(require 'project)
(defvar eglot-server-programs)

;; --- Python ---
(defun my/python-venv-autoload ()
  "Автоматически активирует .venv в корне проекта, если он существует.
Меняет `exec-path` и `process-environment` локально для буфера, чтобы
Flycheck, Eglot и Apheleia использовали локальные ruff/mypy/etc."
  (interactive)
  ;; Пытаемся найти корень проекта
  (when-let* ((project (project-current nil))
              (root (project-root project))
              (venv-bin (expand-file-name ".venv/bin" root)))
    ;; Если папка bin существует
    (when (file-directory-p venv-bin)
      ;; 1. Добавляем в exec-path (чтобы Emacs видел бинарники)
      (setq-local exec-path (cons venv-bin exec-path))
      
      ;; 2. Добавляем в PATH (чтобы запускаемые саб-процессы видели бинарники)
      (setq-local process-environment
                  (cons (concat "PATH=" venv-bin ":" (getenv "PATH"))
                        process-environment))
      
      ;; 3. Говорим python-mode использовать именно этот интерпретатор
      (setq-local python-shell-interpreter (expand-file-name "python" venv-bin))
      
      (message "Activated local venv: %s" venv-bin))))

(use-package python-mode
  :ensure t
  :hook
  ((python-mode python-ts-mode) . my/python-venv-autoload)
  ((python-mode python-ts-mode) . eglot-ensure)
  :bind
  (:map python-mode-map ("C-c C-p" . nil)) ;; Отключаем run-python, если мешает
  :config
  (add-hook 'python-mode-hook #'apheleia-mode)
  
  ;; Настройка сервера для Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((python-ts-mode python-mode) . ("ty" "server")))))

;; --- Java (JDTLS) ---

(defun my-eglot-jdtls-contact (_interactive)
  "Generate a unique JDTLS workspace path for the current project.
Argument _INTERACTIVE is ignored."
  (let* ((project-root (expand-file-name (project-root (project-current t))))
         (workspace-id (md5 project-root))
         (workspace-dir (expand-file-name workspace-id (locate-user-emacs-file "jdtls-workspace"))))
    (unless (file-directory-p workspace-dir)
      (make-directory workspace-dir t))
    (list "jdtls" "-data" workspace-dir)))

;; Конфигурация Java режимов
(use-package java-mode
  :ensure nil ;; Встроено в Emacs
  :init
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  :hook
  ((java-mode java-ts-mode) . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((java-mode java-ts-mode) . my-eglot-jdtls-contact))))

;; --- Scala ---
(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode)
  :hook
  (scala-mode . eglot-ensure)
  (scala-mode . (lambda ()
                  ;; Убираем 'idle-change из списка триггеров
                  ;; Оставляем только 'save (при сохранении) и 'mode-enabled (при открытии)
                  (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(scala-mode . ("metals" :initializationOptions (:isHttpEnabled t))))))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; Оставляем только нужную опцию, старый хак удален
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; --- Rust ---
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil
        rustic-lsp-client 'eglot)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; --- Markdown & Data Formats ---

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t)

(use-package yaml-pro
  :ensure t
  :hook (yaml-mode . yaml-pro-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.protobuf\\'" . protobuf-mode)))

(provide 'lang-prog)
;;; lang-prog.el ends here
