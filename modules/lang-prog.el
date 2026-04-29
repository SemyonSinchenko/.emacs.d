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
Eglot и Apheleia использовали локальные ruff/mypy/etc."
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

(defun my-find-java21-home ()
  "Return the newest SDKMAN Java 21 Zulu installation directory."
  (let* ((base (expand-file-name "~/.sdkman/candidates/java/"))
         (candidates
          (directory-files base t "^[0-9].*-zulu$"))
         (java21-candidates
          (seq-filter
           (lambda (path)
             (string-match-p
              (rx "/" "21" (? "." (+ digit)) (? "." (+ digit)) "-zulu" string-end)
              path))
           candidates))
         (sorted
          (sort java21-candidates #'string>)))
    (or (car sorted)
        (error "No Java 21 Zulu installation found in %s" base))))

(defun my-eglot-jdtls-contact (_interactive)
  "Generate a unique JDTLS workspace path for the current project.
Run JDTLS itself on Java 21, without changing the project's own JDK."
  (let* ((project-root (expand-file-name (project-root (project-current t))))
         (workspace-id (md5 project-root))
         (workspace-dir (expand-file-name
                         workspace-id
                         (locate-user-emacs-file "jdtls-workspace")))
         (java21-home (my-find-java21-home))
         (java21-java (expand-file-name "bin/java" java21-home))
         (java21-bin  (expand-file-name "bin" java21-home)))
    (unless (file-exists-p java21-java)
      (error "Java 21 not found for JDTLS: %s" java21-java))
    (unless (file-directory-p workspace-dir)
      (make-directory workspace-dir t))
    (list "env"
          (concat "JAVA_HOME=" java21-home)
          (concat "PATH=" java21-bin path-separator (getenv "PATH"))
          "jdtls"
          "-data" workspace-dir)))

(use-package java-mode
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  :hook
  ((java-mode java-ts-mode) . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (setf (alist-get '(java-mode java-ts-mode)
                     eglot-server-programs
                     nil nil #'equal)
          #'my-eglot-jdtls-contact)))

;; --- Scala (Metals v1) ---

(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode)
  :hook
  (scala-mode . eglot-ensure)
  (scala-mode . (lambda ()
                  (setq-local flymake-no-changes-timeout 2.0)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(scala-mode . ("metals" :initializationOptions (:isHttpEnabled t))))))

(defun my/eglot-metals-build-import ()
  "Run Metals `build-import` via Eglot asynchronously in the current workspace."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "Current buffer is not managed by Eglot"))
  (let ((server (eglot-current-server)))
    (jsonrpc-async-request
     server
     :workspace/executeCommand
     '(:command "build-import" :arguments [])
     :success-fn
     (lambda (result)
       (message "Metals build-import finished: %S" result))
     :error-fn
     (lambda (err)
       (message "Metals build-import failed: %S" err))))
  (message "Sent Metals command asynchronously: build-import"))

(use-package jarchive
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'eglot (jarchive-mode 1)))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
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

;; Easky
(use-package easky
  :defer t
  :ensure t)

;; Clojure stuff
(use-package clojure-ts-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t)

(provide 'lang-prog)
;;; lang-prog.el ends here
