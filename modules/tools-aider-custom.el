;;; tools-aider-custom.el --- Custom CLI wrapper for Aider via Vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; A lightweight wrapper around 'aider' CLI.
;; Simplified Logic: ALWAYS runs in --architect mode.
;; - --model is set to the Architect model.
;; - --editor-model is set to the Editor model.
;; - Window logic: Smart split (like Magit/display-buffer).
;; Uses Alibaba Coding Plan (DashScope) as the backend.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'transient)

(defgroup my-aider nil
  "Custom Aider configuration."
  :group 'tools)

;; --- 1. Model settings ---

(defcustom my-aider-program "aider"
  "Path to the aider executable."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-architect-model "openai/qwen3.5-plus"
  "The \='Thinking\=' model.  In architect mode, this is passed as --model."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-editor-model "openai/qwen3.5-plus"
  "The \='Coding\=' model.  Passed as --editor-model."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-weak-model "openai/qwen3.5-plus"
  "Weak model (--weak-model) for simple tasks."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-api-base "https://coding-intl.dashscope.aliyuncs.com/v1"
  "The OpenAI-compatible API base URL for Alibaba Coding Plan."
  :type 'string
  :group 'my-aider)

(defcustom my-aider-args
  '("--no-attribute-co-authored-by" ;; No git attribute
    "--no-analytics"                ;; Disable analytics
    "--no-git-commit-verify"        ;; No hooks
    "--dark-mode"                   ;; Enable dark mode for terminal
    "--no-gui"                      ;; Disable TUI
    "--show-model-warnings"         ;; Show model warnings
    "--show-diffs"                  ;; Show text diffs
    "--multiline"                   ;; Enable multiline input mode
    "--edit-format" "diff"          ;; Use diff edit format
    "--editor-edit-format" "diff")  ;; Use diff edit format for editor
  "List of additional arguments to pass to aider."
  :type '(repeat string)
  :group 'my-aider)

;; --- 2. Launch logic ---

(defun my/aider-get-command ()
  "Build the full aider command string.
ALWAYS: aider --architect --model <ARCHITECT> --editor-model <EDITOR> ...
Uses Alibaba Coding Plan API with key from BAILIAN_CODING_PLAN_API_KEY env var."
  (let* ((api-key (or (getenv "BAILIAN_CODING_PLAN_API_KEY") ""))
         (cmd (list my-aider-program)))

    ;; 1. API base and key
    (push "--openai-api-base" cmd)
    (push my-aider-api-base cmd)
    (push "--openai-api-key" cmd)
    (push api-key cmd)

    ;; 2. Always enable architect mode
    (push "--architect" cmd)

    ;; 3. Architect Model (passed as --model)
    (when (and my-aider-architect-model (not (string-empty-p my-aider-architect-model)))
      (push "--model" cmd)
      (push my-aider-architect-model cmd))

    ;; 4. Editor Model
    (when (and my-aider-editor-model (not (string-empty-p my-aider-editor-model)))
      (push "--editor-model" cmd)
      (push my-aider-editor-model cmd))

    ;; 5. Weak Model
    (when (and my-aider-weak-model (not (string-empty-p my-aider-weak-model)))
      (push "--weak-model" cmd)
      (push my-aider-weak-model cmd))

    ;; Remaining arguments
    (setq cmd (append (reverse cmd) my-aider-args))

    ;; Join into a string
    (mapconcat #'identity cmd " ")))

(defun my/aider-run ()
  "Open Aider vterm buffer in the project root.
Always opens in the right window of a split, or creates a split if needed."
  (interactive)
  (let* ((project (project-current nil))
         (root (if project (project-root project) default-directory))
         (buffer-name (format "*aider:%s*" (file-name-nondirectory (directory-file-name root))))
         (current-window (selected-window)))

    ;; 1. Current window has a right neighbor → use it
    ;; 2. Current window has a left neighbor (we're already the right window) → stay put
    ;; 3. No neighbors at all → split right and use the new window
    (cond
     ((window-right current-window)
      (select-window (window-right current-window)))
     ((window-left current-window)
      ;; We are already the right window, stay here
      )
     (t
      (select-window (split-window-right))))

    ;; If buffer already exists, just switch to it; otherwise create and launch aider
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (switch-to-buffer (vterm buffer-name))
      (vterm-send-string (format "cd %s\n" root))
      (vterm-send-string (concat (my/aider-get-command) "\n")))))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "S-<return>")
              (lambda ()
                (interactive)
                (vterm-insert "\n"))))

;; --- 3. Transient Menu ---

(defun my/aider--get-buffer ()
  "Return the active aider vterm buffer for the current project, or nil."
  (let* ((project (project-current nil))
         (root (if project (project-root project) default-directory))
         (buffer-name (format "*aider:%s*" (file-name-nondirectory (directory-file-name root)))))
    (get-buffer buffer-name)))

(defun my/aider--send-to-session (str)
  "Send STR to the active aider vterm buffer."
  (if-let ((buf (my/aider--get-buffer)))
      (with-current-buffer buf
        (vterm-send-string (concat str "\n")))
    (user-error "No active Aider session for this project")))

(defun my/aider--read-project-file (prompt)
  "Prompt for a file in the current project with PROMPT, return relative path."
  (let* ((project (project-current t))
         (root (project-root project))
         (files (project-files project))
         (relative-files (mapcar (lambda (f) (file-relative-name f root)) files))
         (chosen (completing-read prompt relative-files nil t)))
    chosen))

(defun my/aider-add-file ()
  "Send /add <file> to the active Aider session."
  (interactive)
  (let ((file (my/aider--read-project-file "Add file to Aider: ")))
    (my/aider--send-to-session (concat "/add " file))))

(defun my/aider-add-readonly-file ()
  "Send /read-only <file> to the active Aider session."
  (interactive)
  (let ((file (my/aider--read-project-file "Add read-only file to Aider: ")))
    (my/aider--send-to-session (concat "/read-only " file))))

(defun my/aider-clear ()
  "Send /clear to the active Aider session to clear the conversation history."
  (interactive)
  (my/aider--send-to-session "/clear"))

(defun my/aider-drop ()
  "Send /drop to the active Aider session to remove all files from context."
  (interactive)
  (my/aider--send-to-session "/drop"))

(defun my/aider-exit ()
  "Send /exit to the active Aider session, then kill its buffer and window."
  (interactive)
  (if-let ((buf (my/aider--get-buffer)))
      (progn
        (with-current-buffer buf
          (vterm-send-string "/exit\n"))
        ;; Give aider a moment to exit cleanly before killing the buffer
        (run-with-timer
         0.5 nil
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (let ((win (get-buffer-window buffer)))
               (when-let ((proc (get-buffer-process buffer)))
                 (set-process-query-on-exit-flag proc nil))
               (kill-buffer buffer)
               (when (and win (window-live-p win))
                 (delete-window win)))))
         buf))
    (user-error "No active Aider session for this project")))

(transient-define-prefix my/aider-menu ()
  "Aider command menu."
  ["Aider"
   ["Session"
    ("r" "Run Aider" my/aider-run)
    ("c" "Clear history" my/aider-clear)
    ("x" "Exit" my/aider-exit)]
   ["Files"
    ("a" "Add file" my/aider-add-file)
    ("R" "Add read-only file" my/aider-add-readonly-file)
    ("d" "Drop context" my/aider-drop)]])

(provide 'tools-aider-custom)
;;; tools-aider-custom.el ends here
