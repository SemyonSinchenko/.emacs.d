# AGENTS.md - Emacs Configuration Agent Guidelines

This is an Emacs Lisp configuration repository. It uses a modular structure with configuration modules in `modules/` and library code in `lisp/`.

## Project Structure

```
.emacs.d/
├── init.el              # Main entry point - loads all modules
├── modules/             # Configuration modules (core-*, lang-*, tools-*)
│   ├── core-package.el  # Package management setup
│   ├── core-ui.el       # UI configuration
│   ├── core-keys.el     # Global keybindings
│   ├── core-completion.el
│   ├── lang-lsp.el      # LSP, linting, formatting
│   ├── lang-prog.el     # Language-specific settings
│   └── tools-*.el       # Tool configurations (ai, org, dired, rss)
├── lisp/                # Custom library code
│   ├── llm-tool-collection.el
│   └── ...
└── custom.el            # Auto-generated customizations
```

## Build/Lint/Test Commands

This is an Emacs configuration, not a compiled project. There is no formal build process.

### Loading and Testing Changes

To test changes to the configuration:

1. **Load a specific file in Emacs**:
   ```
   M-x load-file RET /path/to/file.el
   ```

2. **Evaluate a buffer**:
   ```
   M-x eval-buffer RET
   ```

3. **Evaluate a region**:
   ```
   Select region, M-x eval-region RET
   ```

4. **Reload entire config**:
   ```
   M-x load-file RET ~/.emacs.d/init.el
   ```

### Linting and Formatting

This project uses **Flycheck** for linting and **Apheleia** for formatting (configured in `modules/lang-lsp.el`).

- **Flycheck**: Automatic syntax checking in Emacs. Enable with `M-x global-flycheck-mode`.
- **Apheleia**: Format code with `C-x x f` (globally bound in `core-keys.el`).

For external linting, you can use:

```bash
# Check Elisp syntax (basic)
emacs --batch --eval "(setq load-path (cons \".\" load-path))" -l file.el --eval "(message \"OK\")"

# Using package-lint (install via MELPA)
emacs --batch -l package-lint -f package-lint-current-file file.el
```

### Running a Single Test

There is no formal test suite in this configuration. If you add tests (using `ert`), run them with:

```elisp
M-x ert RET t RET  ; Run all tests
M-x ert RET test-name RET  ; Run specific test
```

Or from command line:
```bash
emacs --batch -l ert -l your-tests.el -f ert-run-tests-batch-and-exit
```

## Code Style Guidelines

### File Header Convention

All `.el` files should have the standard Emacs Lisp header:

```elisp
;;; filename.el --- Short description -*- lexical-binding: t; -*-

;;; Commentary:
;; Longer description of what this file does

;;; Code:

;; ... implementation ...

(provide 'filename)
;;; filename.el ends here
```

### Lexical Binding

Always include `lexical-binding: t` at the top of every file:

```elisp
;;; my-module.el --- Description -*- lexical-binding: t; -*-
```

### Imports and Dependencies

- Use `require` to load built-in Emacs features and external packages:
  ```elisp
  (require 'package)
  (require 'use-package)
  ```

- Use `use-package` for external packages (installs automatically if needed):
  ```elisp
  (use-package some-package
    :ensure t
    :custom ((some-var "value"))
    :config
    (some-function))
  ```

- Use `declare-function` to suppress unknown function warnings:
  ```elisp
  (declare-function some-function "some-file")
  ```

### Naming Conventions

- **Prefix user-defined variables/functions with module name**:
  - `my-` for global/user-level (e.g., `my-eglot-map`)
  - Module-specific prefix (e.g., `llm-tool-collection-*` in lisp/)

- **Use kebab-case for names**: `my-custom-function`, `some-variable`

- **Private functions use `--` prefix**: `llm-tool-collection--view-text`

### Keybinding Definitions

Use `defvar` with `make-sparse-keymap` or `defvar-keymap` (Emacs 29+):

```elisp
(defvar my-module-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c x" #'some-command)
    map)
  "Description of keymap.")

(keymap-global-set "C-x M-." my-module-map)
```

### Use-package Style

Follow this pattern for package configuration:

```elisp
(use-package package-name
  :ensure t
  :defer t                    ; Lazy load
  :bind ("C-c k" . command)  ; Bind keys
  :custom
  (var1 "value1")
  (var2 "value2")
  :config
  (setup-function))
```

### Error Handling

Use `condition-case` for robust error handling:

```elisp
(condition-case err
    (risky-function)
  (error
   (message "Error: %s" (error-message-string err))))
```

Use `when-let*` and `if-let*` for cleaner conditional logic:

```elisp
(if-let* ((buf (get-buffer buffer-name)))
    (do-something buf)
  (error "Buffer not found: %s" buffer-name))
```

### Formatting

- Use **Apheleia** with `C-x x f` to format automatically
- Follow standard Elisp indentation (Emacs does this automatically)
- Keep lines under 80 characters when practical
- Use `\` for line continuation in long lists

### Documentation

- Add docstrings to all public functions:
  ```elisp
  (defun my-function (arg)
    "Describe what ARG does and what this function returns."
    ...)
  ```

- Use Commentary section in file headers for overview
- Comment complex sections in Russian or English (be consistent)

### Defcustom for User Options

For configuration variables users might want to customize:

```elisp
(defcustom my-module-setting "default"
  "Description for users."
  :type 'string
  :group 'my-module)
```

### Common Patterns

**Adding to load-path**:
```elisp
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
```

**Safe local variables** (for dir-locals):
```elisp
(put 'flycheck-checker 'safe-local-variable #'symbolp)
```

**Hook registration**:
```elisp
(add-hook 'some-mode-hook #'my-hook-function)
```

### Common Issues to Avoid

1. Don't use `setq` on variables that should be customizable via `defcustom`
2. Avoid circular dependencies between modules
3. Don't load heavy packages at startup unless needed (use `:defer t`)
4. Use `add-to-list` with `t` for appending to preserve order:
  ```elisp
  (add-to-list 'some-list "value" t)
  ```

## Common Tasks

### Adding a New Module

1. Create `modules/xxx-yyy.el` with proper header
2. Add `(require 'xxx-yyy)` to `init.el` in appropriate order
3. Follow naming conventions and use-package pattern

### Adding a Keybinding

1. Edit the relevant module (e.g., `core-keys.el` for global bindings)
2. Use `keymap-set` or `global-set-key`

### Modifying Package Settings

1. Find the relevant module (e.g., `lang-lsp.el` for LSP/formatting)
2. Add customizations in `:custom` section of use-package

## Additional Resources

- [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [Use-package Documentation](https://github.com/jwiegley/use-package)
- [Emacs Rocks - Elisp Style Guide](http://emacsrocks.com/)
