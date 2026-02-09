;;; tools-repomap.el --- Project Map generator for LLM context -*- lexical-binding: t; -*-

;;; Commentary:
;; Generates a concise map of the project structure.
;; Strategy: Extension-based dispatch.
;; 1. Look up config by file extension.
;; 2. Try Tree-sitter (High precision).
;;    - Handles multi-line signatures.
;;    - Truncates overly long lines.
;;    - Removes trailing '=' for Scala.
;; 3. Fallback to Regex (High robustness).
;; 4. Supports .aider.gptelignore for custom exclusion.

;;; Code:

(require 'projectile)
(require 'treesit)
(require 'gptel)
(require 'seq)
(require 'subr-x)

(defgroup my-repomap nil
  "Project Map configuration."
  :group 'tools)

(defcustom my-repomap-file ".aider.gptelmap"
  "Name of the map file to generate in project root."
  :type 'string
  :group 'my-repomap)

(defcustom my-repomap-ignore-file ".aider.gptelignore"
  "Name of the local ignore file (glob patterns)."
  :type 'string
  :group 'my-repomap)

(defcustom my-repomap-debug t
  "If non-nil, print debug info to *Messages*."
  :type 'boolean
  :group 'my-repomap)

(defcustom my-repomap-ignore-extensions
  '("json" "yaml" "yml" "toml" "lock" "txt" "md" "org" "png" "jpg" "svg" "css" "scss" "xml" "properties" "gradle" "gitignore" "pyi")
  "File extensions to ignore when building the map."
  :type '(repeat string)
  :group 'my-repomap)

;; --- 1. Configuration Map (Extension based) ---

(defvar my/repomap-config
  `((("py")
     :lang python
     :regex "^[ \t]*\\(?:async[ \t]+\\)?\\(def\\|class\\)[ \t]+\\([a-zA-Z0-9_]+\\)"
     :ts-query ((function_definition name: (identifier) @name parameters: (parameters) @params) @def
                (class_definition name: (identifier) @name) @def))
    
    (("el")
     :lang elisp
     :regex "^(\\(def[a-z-]+\\|use-package\\)[ \t]+\\(\\S-+\\)"
     :ts-query ((list (symbol) @keyword (symbol) @name) @def
                (:match "^\\(defun\\|defvar\\|defcustom\\|defmacro\\|cl-defun\\|defgroup\\|use-package\\)$" @keyword)))

    (("go")
     :lang go
     :regex "^\\(func\\|type\\)[ \t]+\\([a-zA-Z0-9_]+\\)"
     :ts-query ((function_declaration name: (identifier) @name signature: (parameter_list) @params) @def
                (method_declaration name: (field_identifier) @name parameters: (parameter_list) @params) @def
                (type_declaration (type_spec name: (type_identifier) @name)) @def))

    (("rs")
     :lang rust
     :regex "^[ \t]*\\(fn\\|struct\\|enum\\|trait\\)[ \t]+\\([a-zA-Z0-9_]+\\)"
     :ts-query ((function_item name: (identifier) @name parameters: (parameters) @params) @def
                (struct_item name: (type_identifier) @name) @def
                (enum_item name: (type_identifier) @name) @def))

    (("java")
     :lang java
     :regex "^[ \t]*\\(public\\|private\\|protected\\).*\\(class\\|interface\\|enum\\|void\\|[A-Z]\\w+\\)[ \t]+\\([a-zA-Z0-9_]+\\)[:(]"
     :ts-query ((class_declaration name: (identifier) @name) @def
                (interface_declaration name: (identifier) @name) @def
                (method_declaration name: (identifier) @name parameters: (formal_parameters) @params) @def))

    (("scala" "sc" "sbt")
     :lang scala
     :regex "^[ \t]*\\(def\\|val\\|class\\|trait\\|object\\)[ \t]+\\([a-zA-Z0-9_]+\\)"
     :ts-query ((class_definition name: (identifier) @name) @def
                (object_definition name: (identifier) @name) @def
                (trait_definition name: (identifier) @name) @def
                (function_definition name: (identifier) @name parameters: (parameters) @params) @def)))
  "Configuration for each file extension.")

;; --- 2. Core Logic ---

(defun my/repomap--get-config-for-file (file)
  "Find config based on FILE extension."
  (let ((ext (file-name-extension file)))
    (when ext
      (seq-find (lambda (entry) (member ext (car entry))) my/repomap-config))))

(defun my/repomap--clean-signature (text)
  "Collapse newlines/spaces in TEXT.  Truncate >150 chars.  Remove trailing '='."
  (let* ((clean (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))
         ;; Удаляем = в конце (для Scala)
         (clean-no-eq (string-trim (replace-regexp-in-string "=[ \t]*$" "" clean))))
    
    (if (> (length clean-no-eq) 150)
        (concat (substring clean-no-eq 0 147) "...")
      clean-no-eq)))

(defun my/repomap--get-signature (node)
  "Extract signature from NODE.
If a \='body\=' child exists (Python/Java/Go), cuts text before it.
Otherwise (Elisp), takes the first line."
  (let ((body-node (or (treesit-node-child-by-field-name node "body")           ;; Python
                       (treesit-node-child-by-field-name node "block")          ;; Java/Go/Rust
                       (treesit-node-child-by-field-name node "template_body")) ;; Scala
                   ))
    (if body-node
        ;; Если тело найдено, берем все от начала узла до начала тела
        (let ((start (treesit-node-start node))
              (end (treesit-node-start body-node)))
          (my/repomap--clean-signature (buffer-substring-no-properties start end)))
      
      ;; Fallback: Если тела нет
      (let ((text (treesit-node-text node t)))
        (my/repomap--clean-signature (car (split-string text "\n")))))))

(defun my/repomap--extract-via-ts (lang query)
  "Try to extract definitions from LANG and QUERY using Tree-sitter."
  (condition-case nil
      (let* ((parser (treesit-parser-create lang))
             (root-node (treesit-parser-root-node parser))
             (captures (treesit-query-capture root-node query))
             (results '()))
        (dolist (capture captures)
          ;; Only process @def nodes
          (when (eq (car capture) 'def)
            (let ((sig (my/repomap--get-signature (cdr capture))))
              (when (and sig (not (string-empty-p sig)))
                (push (concat "- " sig) results)))))
        (nreverse results))
    (error nil)))

(defun my/repomap--extract-via-regex (regex)
  "Try to extract definitions using REGEX."
  (let ((results '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((line (string-trim (match-string 0))))
          (push (concat "- " (my/repomap--clean-signature line)) results))))
    (nreverse results)))

(defun my/repomap--extract-definitions (file)
  "Parse FILE based on extension."
  (let ((config (my/repomap--get-config-for-file file)))
    (when config
      (with-temp-buffer
        (insert-file-contents file)
        
        (let* ((ts-lang (plist-get (cdr config) :lang))
               (ts-query (plist-get (cdr config) :ts-query))
               (regex (plist-get (cdr config) :regex))
               (ts-results nil)
               (regex-results nil))
          
          ;; 1. TS Strategy
          (when (and (fboundp 'treesit-language-available-p)
                     (treesit-language-available-p ts-lang))
            (setq ts-results (my/repomap--extract-via-ts ts-lang ts-query))
            (when (and my-repomap-debug ts-results)
              (message "[Map] %s: Parsed via Tree-sitter (%s)" (file-name-nondirectory file) ts-lang)))
          
          ;; 2. Regex Strategy (Fallback)
          (unless ts-results
            (when regex
              (setq regex-results (my/repomap--extract-via-regex regex))
              (when (and my-repomap-debug regex-results)
                (message "[Map] %s: Parsed via Regex" (file-name-nondirectory file)))))
          
          ;; Return
          (let ((final (or ts-results regex-results)))
            (if final
                (string-join final "\n")
              (when my-repomap-debug
                (message "[Map] %s: No definitions found" (file-name-nondirectory file)))
              nil)))))))

;; --- 3. Filtering & Ignore Logic ---

(defun my/repomap--load-ignore-patterns (root)
  "Load ignore patterns from .aider.gptelignore in ROOT."
  (let ((ignore-file (expand-file-name my-repomap-ignore-file root)))
    (if (file-exists-p ignore-file)
        (with-temp-buffer
          (insert-file-contents ignore-file)
          (seq-filter (lambda (line)
                        (and (not (string-empty-p line))
                             (not (string-prefix-p "#" line))))
                      (split-string (buffer-string) "\n" t)))
      nil)))

(defun my/repomap--ignored-p (file ignore-patterns)
  "Check if FILE matches any of IGNORE-PATTERNS (globs)."
  (seq-some (lambda (pat)
              (if (string-suffix-p "/" pat)
                  ;; Если паттерн папка (legacy/), проверяем префикс
                  (string-prefix-p pat file)
                ;; Иначе проверяем как glob (wildcard)
                (string-match-p (wildcard-to-regexp pat) file)))
            ignore-patterns))

(defun my/repomap--collect-files (root)
  "Get list of interesting files from ROOT using Projectile + Custom Ignore."
  (let ((default-directory root)
        (ignore-patterns (my/repomap--load-ignore-patterns root)))
    
    (when (and ignore-patterns my-repomap-debug)
      (message "[Map] Loaded ignore patterns: %s" ignore-patterns))
    
    (seq-filter
     (lambda (f)
       (let ((ext (file-name-extension f)))
         (and ext
              ;; 1. Стандартный список расширений
              (not (member ext my-repomap-ignore-extensions))
              ;; 2. Хардкод для Protobuf (Python)
              (not (string-suffix-p "_pb2.py" f))
              ;; 3. .aider.gptelignore
              (if ignore-patterns
                  (not (my/repomap--ignored-p f ignore-patterns))
                t))))
     (projectile-current-project-files))))

;; --- 4. Public Commands ---

;;;###autoload
(defun my/repomap-generate ()
  "Generate .aider.gptelmap for the current project."
  (interactive)
  (let* ((project (project-current nil))
         (root (if project (project-root project) default-directory))
         (map-file (expand-file-name my-repomap-file root))
         (files (my/repomap--collect-files root))
         (count 0))
    
    (message "Generating project map for %s..." root)
    
    (with-temp-file map-file
      (insert "# Project Map\n")
      (insert (format "# Root: %s\n" root))
      (insert (format "# Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      
      (dolist (file files)
        (let ((abs-path (expand-file-name file root)))
          (when (and (file-exists-p abs-path)
                     (< (file-attribute-size (file-attributes abs-path)) 500000))
            (let ((defs (my/repomap--extract-definitions abs-path)))
              (when defs
                (insert (format "## %s\n" file))
                (insert defs)
                (insert "\n\n")
                (setq count (1+ count))))
            (when (= (% count 20) 0)
              (redisplay)))))
      )
    
    (message "Map generated: %s (%d files processed)" map-file count)))

;;;###autoload
(defun my/repomap-add-context ()
  "Add project map to Gptel context.
If map doesn't exist, ask to generate it first."
  (interactive)
  (let* ((project (project-current nil))
         (root (if project (project-root project) default-directory))
         (map-file (expand-file-name my-repomap-file root)))
    
    (if (file-exists-p map-file)
        ;; Map exists - add to context
        (progn
          (gptel-add-file map-file)
          (message "Project map added to Gptel context"))
      
      ;; Map doesn't exist - ask to generate
      (when (y-or-n-p (format "Project map not found at %s. Generate it now?" map-file))
        (my/repomap-generate)
        (when (file-exists-p map-file)
          (gptel-add-file map-file)
          (message "Project map generated and added to Gptel context"))))))

(provide 'tools-repomap)
;;; tools-repomap.el ends here
