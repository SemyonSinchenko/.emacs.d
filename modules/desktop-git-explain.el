;;; desktop-git-explain.el --- LLM explainer of uncommitted changes -*- lexical-binding: t; -*-

;;; Commentary:
;; `C-c g e' asks "[a]ll changes / [s]elect files" and streams an Org
;; explainer of the uncommitted changes (worktree vs HEAD, untracked
;; files included) from the gptel backends of desktop-ai.el: summary,
;; intent, a walkthrough per file in reading order, how it fits, at
;; most five notes worth a closer look, and how to verify.
;;
;; Files matching `my-desktop-git-explain-exclude' (lockfiles),
;; generated files (.gitattributes `linguist-generated' / `-diff'),
;; diffs above `my-desktop-git-explain-max-file-size' and binaries are
;; skipped by default.  [s]elect opens a Dired-like checklist with
;; those unmarked and a live size vs `my-desktop-git-explain-budget'.
;; Unmarked files are still listed to the model by name and size.
;;
;; One request, no gptel tools: the whole context (diffs, numbered
;; full texts, instruction files, repo file list, recent commits) is
;; sent up front.  Over budget, full texts go first, then function
;; context.  `C-u C-c g e' also asks for the intended change.
;;
;; The explainer opens maximized like the C-c g diffs.  In it: q
;; quit (restores the layout), g regenerate (C-u g reselect files),
;; d difftastic diff of the file heading at point, k cancel.  Cited
;; `path:line' become file links; file headings get a [diff] link
;; (`difft:' link type).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'desktop-config-defs)
(require 'desktop-core)
(require 'desktop-project)

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-context)
(defvar gptel-use-context)
(defvar gptel-include-reasoning)
(defvar gptel-org-convert-response)
(defvar my-ai--backends)
(declare-function gptel-request "gptel-request")
(declare-function gptel-abort "gptel-request" (buf))
(declare-function gptel-backend-name "gptel-request" (backend))
(declare-function gptel-backend-models "gptel-request" (backend))
(declare-function gptel--convert-markdown->org "gptel-org" (str))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-up-heading-safe "org" ())
(declare-function org-in-src-block-p "org" (&optional inside element))
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function outline-hide-subtree "outline" ())
(defvar org-link-bracket-re)

;; ------------------------------------------------------------------
;; Prompt
;; ------------------------------------------------------------------

(defconst my-git-explain--system-prompt
  "You explain an uncommitted code change to its owner, who will decide
whether to commit it.  An AI agent wrote the change; the owner did not
read it yet.  Goal: the owner understands it in minutes and knows where
to look closer.

Rules:
- Explain only what the material shows.  If something depends on code
  you were not given, say so; never guess from names.
- Cite evidence as `path:line` using the line numbers in <file> blocks
  (new version) or the hunk headers of <diff>.  For removed code, cite
  the file and say \"removed\".
- Files tagged (not shown) in <diffstat> were left out by the owner:
  mention them only from their name and size, never guess their content.
- Order the walkthrough for reading, not alphabetically: start where
  behaviour enters (commands, entry points, config), then what they
  call.  Collapse trivial edits (renames, comments, formatting) into
  one final line.
- Be concrete and brief.  No praise, no line-by-line restating of the
  diff, no generic advice.
- \"Worth a closer look\" is not a style review.  Only: behaviour or
  correctness risks, security, data loss, violations of the project
  instructions, or decisions the owner must make.  At most 5 items,
  most important first.  Tag each [Important], [Question] or
  [Pre-existing] (a problem the change did not introduce).  If nothing
  qualifies, write exactly: \"Nothing stands out.\"
- When <scope> is a single file, the walkthrough has exactly one file
  heading; the other changed files are context only.
- Write in %s.  Output Markdown with exactly these headings:

# Summary
First line: \"<N> files changed, <M> notes worth a look.\"  Then 2-4
sentences: what the change does, in plain words.
# Intent
The problem it appears to solve.  If <intent> is given, say whether
the change matches it and what is missing or extra.
# Walkthrough
One \"## `path` - <its role in this change>\" per file, in reading
order: what changed, why it matters, what it connects to
(callers/callees with `path:line`).
# How it fits
Where this sits in the architecture; conventions from <instructions>
followed or broken; any new pattern others might copy.
# Worth a closer look
Numbered items: \"[Tag] claim - evidence `path:line` - what to check\".
# How to verify
Concrete commands or manual steps to confirm it works."
  "System prompt of the explainer; %s is the output language.")

(defconst my-git-explain--repo-files-max 1500
  "Maximum number of repository paths listed in <repo_files>.")

(defconst my-git-explain--empty-tree
  "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "Git's empty tree: the diff base of a repository without commits.")

(defconst my-git-explain--levels
  '((full . "diffs + full files")
    (mid . "diffs only")
    (min . "plain diffs"))
  "Context levels tried in order until the request fits the budget.")

;; ------------------------------------------------------------------
;; Collecting the change
;; ------------------------------------------------------------------

(defun my-git-explain--git (&rest args)
  "Return the output of `git ARGS' in `default-directory'."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil args)
    (buffer-string)))

(defun my-git-explain--base ()
  "Return the diff base: HEAD, or the empty tree without commits."
  (if (zerop (process-file "git" nil nil nil
                           "rev-parse" "--verify" "-q" "HEAD"))
      "HEAD"
    my-git-explain--empty-tree))

(defun my-git-explain--excluded-p (path)
  "Return non-nil when PATH matches `my-desktop-git-explain-exclude'."
  (seq-some (lambda (pattern)
              (string-match-p (wildcard-to-regexp pattern)
                              (if (string-search "/" pattern)
                                  path
                                (file-name-nondirectory path))))
            my-desktop-git-explain-exclude))

(defun my-git-explain--generated (paths)
  "Return the PATHS that .gitattributes marks as generated."
  (when paths
    (let ((fields (split-string
                   (apply #'my-git-explain--git "check-attr" "-z"
                          "linguist-generated" "diff" "--" paths)
                   "\0"))
          generated)
      (while (cddr fields)
        (pcase-let ((`(,path ,attr ,value) fields))
          (when (or (and (equal attr "linguist-generated")
                         (member value '("set" "true")))
                    (and (equal attr "diff") (equal value "unset")))
            (push path generated)))
        (setq fields (nthcdr 3 fields)))
      generated)))

(defun my-git-explain--binary-file-p (file)
  "Return non-nil when FILE has a NUL byte in its first 8000 bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 8000)
    (goto-char (point-min))
    (search-forward "\0" nil t)))

(defun my-git-explain--numbered (file)
  "Return the text of FILE with right-aligned line numbers."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n")))
      (when (equal (car (last lines)) "")
        (setq lines (butlast lines)))
      (let ((n 0))
        (mapconcat (lambda (line)
                     (format "%5d│ %s" (setq n (1+ n)) line))
                   lines "\n")))))

(defun my-git-explain--entry (base change generated)
  "Return the entry plist of CHANGE, a (PATH STATUS ORIG) list.
BASE is the diff base; GENERATED the paths marked as generated."
  (pcase-let* ((`(,path ,status ,orig) change)
               (pathspec (delq nil (list path orig)))
               (untracked (equal status "??"))
               (new (or untracked (eq (aref status 0) ?A)))
               (exists (file-exists-p path))
               (numstat (unless untracked
                          (apply #'my-git-explain--git "diff" base "-M"
                                 "--numstat" "--" pathspec)))
               (binary (if untracked
                           (my-git-explain--binary-file-p path)
                         (string-prefix-p "-\t-" numstat)))
               (counts (unless (or untracked binary)
                         (split-string numstat "\t")))
               (text (and exists (not binary)
                          (my-git-explain--numbered path)))
               (diff-args (list base "-M" "--no-color" "--no-ext-diff"))
               (fctx (unless (or new binary)
                       (apply #'my-git-explain--git "diff"
                              (append diff-args '("--function-context" "--")
                                      pathspec))))
               (plain (unless (or new binary)
                        (apply #'my-git-explain--git "diff"
                               (append diff-args '("--") pathspec))))
               (size-min (if new (length text) (length plain))))
    (list :path path :status status :orig orig :new new :binary binary
          :added (cond (counts (string-to-number (nth 0 counts)))
                       (text (with-temp-buffer
                               (insert text)
                               (count-lines (point-min) (point-max)))))
          :deleted (and counts (string-to-number (nth 1 counts)))
          :text text :fctx fctx :plain plain
          :size (list (cons 'full (if new (length text)
                                    (+ (length fctx) (length text))))
                      (cons 'mid (if new (length text) (length fctx)))
                      (cons 'min size-min))
          :skip (cond (binary "binary")
                      ((my-git-explain--excluded-p path) "excluded")
                      ((member path generated) "generated")
                      ((> size-min my-desktop-git-explain-max-file-size)
                       "large")))))

(defun my-git-explain--collect ()
  "Return entry plists of the changes at `default-directory'.
Signal a user error when there is nothing to explain."
  (let* ((changes (or (my-git--changed-files)
                      (user-error "No changes to explain")))
         (base (my-git-explain--base))
         (generated (my-git-explain--generated (mapcar #'car changes))))
    (mapcar (lambda (change)
              (my-git-explain--entry base change generated))
            changes)))

(defun my-git-explain--default-marks (entries)
  "Return the paths of ENTRIES not skipped by default."
  (mapcar (lambda (e) (plist-get e :path))
          (seq-remove (lambda (e) (plist-get e :skip)) entries)))

(defun my-git-explain--cap (string max)
  "Return STRING cut to MAX characters, noting the cut."
  (if (<= (length string) max)
      string
    (concat (substring string 0 max)
            (format "\n[... %d more characters not sent]"
                    (- (length string) max)))))

(defun my-git-explain--context ()
  "Return the fixed context blocks at `default-directory' as a plist.
:project, :head, :instructions (list of (FILE . TEXT)), :commits,
:repo-files."
  (let* ((head (string-trim
                (my-git-explain--git "log" "-1" "--format=%h %s")))
         (files (split-string (my-git-explain--git "ls-files") "\n" t))
         (shown (seq-take files my-git-explain--repo-files-max)))
    (list :project (file-name-nondirectory
                    (directory-file-name default-directory))
          :head (if (string-empty-p head) "(no commits)" head)
          :instructions
          (delq nil
                (mapcar (lambda (name)
                          (when (file-readable-p name)
                            (cons name
                                  (my-git-explain--cap
                                   (with-temp-buffer
                                     (insert-file-contents name)
                                     (buffer-string))
                                   my-desktop-git-explain-instructions-max))))
                        my-desktop-git-explain-instructions))
          :commits (string-trim
                    (my-git-explain--git "log" "-10" "--oneline"))
          :repo-files
          (concat (string-join shown "\n")
                  (when (> (length files) (length shown))
                    (format "\n[... %d more files]"
                            (- (length files) (length shown))))))))

;; ------------------------------------------------------------------
;; Building the request
;; ------------------------------------------------------------------

(defun my-git-explain--size-label (chars)
  "Return CHARS as a short KB string."
  (format "%d KB" (max 1 (round chars 1000))))

(defun my-git-explain--diffstat-line (entry marked)
  "Return the <diffstat> line of ENTRY; MARKED is the sent paths."
  (let ((path (plist-get entry :path)))
    (concat path "  "
            (cond ((plist-get entry :binary) "binary")
                  ((plist-get entry :new)
                   (format "+%d" (or (plist-get entry :added) 0)))
                  (t (format "+%d -%d" (or (plist-get entry :added) 0)
                             (or (plist-get entry :deleted) 0))))
            "  " (my-git--status-label (plist-get entry :status)
                                       (plist-get entry :orig))
            (unless (member path marked)
              (format "  (not shown: %s, %s)"
                      (or (plist-get entry :skip) "not selected")
                      (my-git-explain--size-label
                       (alist-get 'min (plist-get entry :size))))))))

(defun my-git-explain--scope (entries marked)
  "Return the <scope> text for MARKED paths of ENTRIES."
  (cond
   ((= (length marked) (length entries)) "all uncommitted changes")
   ((= (length marked) 1) (format "the single file %s" (car marked)))
   (t (format "the selected files: %s" (string-join marked ", ")))))

(defun my-git-explain--prompt (ctx entries marked level intent)
  "Return the user prompt for MARKED paths of ENTRIES at LEVEL.
CTX is from `my-git-explain--context'; INTENT a string or nil."
  (let ((sent (seq-filter (lambda (e) (member (plist-get e :path) marked))
                          entries)))
    (concat
     (format "<project name=\"%s\">\nHEAD: %s\n</project>\n"
             (plist-get ctx :project) (plist-get ctx :head))
     (format "<scope>%s</scope>\n"
             (my-git-explain--scope entries marked))
     (format "<intent>%s</intent>\n" (or intent "not stated"))
     (mapconcat (lambda (inst)
                  (format "<instructions file=\"%s\">\n%s\n</instructions>\n"
                          (car inst) (cdr inst)))
                (plist-get ctx :instructions) "")
     (format "<recent_commits>\n%s\n</recent_commits>\n"
             (plist-get ctx :commits))
     (format "<repo_files>\n%s\n</repo_files>\n"
             (plist-get ctx :repo-files))
     (format "<diffstat>\n%s\n</diffstat>\n"
             (mapconcat (lambda (e) (my-git-explain--diffstat-line e marked))
                        entries "\n"))
     (format "<diff>\n%s</diff>\n"
             (mapconcat (lambda (e)
                          (or (plist-get e (if (eq level 'min) :plain :fctx))
                              ""))
                        sent ""))
     (mapconcat
      (lambda (e)
        (when (and (plist-get e :text)
                   (or (plist-get e :new) (eq level 'full)))
          (format "<file path=\"%s\" status=\"%s\">\n%s\n</file>\n"
                  (plist-get e :path)
                  (if (plist-get e :new) "new" "modified")
                  (plist-get e :text))))
      sent "")
     (format "Explain %s." (my-git-explain--scope entries marked)))))

(defun my-git-explain--system ()
  "Return the system prompt in the configured language."
  (format my-git-explain--system-prompt my-desktop-git-explain-language))

(defun my-git-explain--fit (ctx entries marked)
  "Return (LEVEL . SIZE): the first context level that fits the budget.
LEVEL is nil when even plain diffs do not fit; SIZE is then the
smallest request size.  CTX, ENTRIES and MARKED as for the prompt."
  (let ((fixed (+ (length (my-git-explain--system))
                  (length (my-git-explain--prompt ctx entries nil 'min nil))))
        (sent (seq-filter (lambda (e) (member (plist-get e :path) marked))
                          entries))
        result)
    (dolist (level (mapcar #'car my-git-explain--levels))
      (unless (car result)
        (let ((size (apply #'+ fixed
                           (mapcar (lambda (e)
                                     (alist-get level (plist-get e :size)))
                                   sent))))
          (setq result (cons (and (<= size my-desktop-git-explain-budget)
                                  level)
                             size)))))
    result))

;; ------------------------------------------------------------------
;; Backend
;; ------------------------------------------------------------------

(defun my-git-explain--backend ()
  "Return (BACKEND . MODEL) for explainers, or signal a user error."
  (unless (and (require 'gptel nil t) (bound-and-true-p my-ai--backends))
    (user-error (concat "No AI backend: enable the AI module and set a key"
                        " (C-c a a reports which)")))
  (let* ((backend (if my-desktop-git-explain-backend
                      (or (cdr (assoc my-desktop-git-explain-backend
                                      (bound-and-true-p my-ai--backends)))
                          (user-error "AI backend %s is not available"
                                      my-desktop-git-explain-backend))
                    (default-value 'gptel-backend)))
         (model (or my-desktop-git-explain-model
                    (and my-desktop-git-explain-backend
                         (let ((m (car (gptel-backend-models backend))))
                           (if (consp m) (car m) m)))
                    (default-value 'gptel-model))))
    (cons backend model)))

;; ------------------------------------------------------------------
;; Explainer buffer
;; ------------------------------------------------------------------

(defvar-local my-git-explain--state nil
  "Plist of the explainer in this buffer.
:root, :entries, :marked (list of paths, or `all'), :intent, :ctx,
:level, :size, :backend, :model, :marker, :started, :status.")

(defvar-local my-git-explain--active nil
  "Non-nil while a request of this explainer buffer is running.")

(defun my-git-explain--only-read-only (command)
  "Return COMMAND when the buffer is read-only (a key filter)."
  (and buffer-read-only command))

(defvar my-git-explain-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Plain keys act only while read-only: after C-x C-q they type.
    (dolist (binding '(("q" . my-git-explain-quit)
                       ("g" . my-git-explain-regenerate)
                       ("d" . my-git-explain-diff)
                       ("k" . my-git-explain-cancel)))
      (keymap-set map (car binding)
                  `(menu-item "" ,(cdr binding)
                              :filter my-git-explain--only-read-only)))
    map)
  "Keymap of `my-git-explain-mode'.")

(define-minor-mode my-git-explain-mode
  "Minor mode of C-c g e explainer buffers.
\\{my-git-explain-mode-map}"
  :lighter " Explain")

(defun my-git-explain--buffer-name (project marked entries)
  "Return the explainer buffer name for PROJECT and MARKED of ENTRIES."
  (if (and (= (length marked) 1) (> (length entries) 1))
      (format "*explain: %s: %s*" project (car marked))
    (format "*explain: %s*" project)))

(defun my-git-explain--header-line ()
  "Update the header line from the buffer's state."
  (let* ((st my-git-explain--state)
         (status (plist-get st :status)))
    (setq header-line-format
          (concat
           " "
           (pcase status
             ('running (propertize "Generating…" 'face 'warning))
             ('cancelled (propertize "Cancelled" 'face 'warning))
             (`(done . ,secs)
              (propertize (format "Done in %ds" secs) 'face 'success))
             (`(error . ,msg)
              (propertize (format "Error: %s" msg) 'face 'error)))
           (propertize
            (format " · %s/%s · %s · %s"
                    (gptel-backend-name (plist-get st :backend))
                    (plist-get st :model)
                    (let ((n (length (plist-get st :sent))))
                      (format "%d file%s" n (if (= n 1) "" "s")))
                    (alist-get (plist-get st :level) my-git-explain--levels))
            'face 'shadow)
           (propertize "   q quit · g regenerate · C-u g reselect · d diff · k cancel"
                       'face 'shadow)))
    (force-mode-line-update)))

(defun my-git-explain--set-status (status)
  "Set the explainer STATUS and refresh the header line."
  (setq my-git-explain--state
        (plist-put my-git-explain--state :status status))
  (my-git-explain--header-line))

(defun my-git-explain--insert-header ()
  "Insert the Org header of the explainer from its state."
  (let* ((st my-git-explain--state)
         (ctx (plist-get st :ctx))
         (entries (plist-get st :entries))
         (sent (plist-get st :sent))
         (scope (my-git-explain--scope entries sent)))
    (insert ":PROPERTIES:\n"
            (format ":ROOT:    %s\n"
                    (abbreviate-file-name (plist-get st :root)))
            (format ":HEAD:    %s\n" (plist-get ctx :head))
            (format ":SCOPE:   %s (%d of %d files, %s)\n"
                    scope (length sent) (length entries)
                    (alist-get (plist-get st :level)
                               my-git-explain--levels))
            (format ":MODEL:   %s / %s\n"
                    (gptel-backend-name (plist-get st :backend))
                    (plist-get st :model))
            (format ":CREATED: %s\n"
                    (format-time-string "[%Y-%m-%d %a %H:%M]"))
            ":END:\n"
            (format "#+title: Explain — %s — %s\n"
                    (plist-get ctx :project)
                    (if (= (length sent) 1) (car sent) scope))
            "#+startup: showall\n"
            (if (plist-get st :intent)
                (format "#+intent: %s\n" (plist-get st :intent))
              "")
            "\n")))

(defun my-git-explain--insert (text)
  "Insert response TEXT at the explainer's response marker."
  (let ((inhibit-read-only t)
        (marker (plist-get my-git-explain--state :marker)))
    (save-excursion
      (goto-char marker)
      (insert text)
      (set-marker marker (point)))))

(defun my-git-explain--callback (buffer)
  "Return the gptel callback streaming into explainer BUFFER."
  (lambda (response info)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase response
          ((pred stringp)
           (my-git-explain--insert
            (cond ((not (plist-get info :stream))
                   (gptel--convert-markdown->org response))
                  ((plist-get info :transformer)
                   (funcall (plist-get info :transformer) response))
                  (t response)))
           (unless (plist-get info :stream)
             (my-git-explain--finish)))
          ('t (my-git-explain--finish))
          ('abort
           (setq my-git-explain--active nil)
           (my-git-explain--set-status 'cancelled))
          ('nil
           (setq my-git-explain--active nil)
           (my-git-explain--set-status
            (cons 'error (or (plist-get info :status) "no response")))
           (message "Explainer failed: %s" (plist-get info :status)))
          ;; Reasoning and tool chunks are not shown.
          (_ nil))))))

(defun my-git-explain--send ()
  "Send the explainer request of the current buffer."
  (let* ((st my-git-explain--state)
         (buffer (current-buffer))
         (prompt (my-git-explain--prompt
                  (plist-get st :ctx) (plist-get st :entries)
                  (plist-get st :sent) (plist-get st :level)
                  (plist-get st :intent))))
    (require 'gptel-org)
    ;; Buffer-local and let-bound: gptel builds the request in a copy
    ;; of this buffer, partly asynchronously.  No tools, no context.
    (setq-local gptel-backend (plist-get st :backend)
                gptel-model (plist-get st :model)
                gptel-stream t
                gptel-tools nil
                gptel-use-tools nil
                gptel-context nil
                gptel-use-context nil
                gptel-include-reasoning 'ignore
                gptel-org-convert-response t)
    (let ((gptel-backend (plist-get st :backend))
          (gptel-model (plist-get st :model))
          (gptel-tools nil)
          (gptel-use-tools nil)
          (gptel-use-context nil))
      (setq my-git-explain--active t)
      (gptel-request prompt
        :buffer buffer
        :position (plist-get st :marker)
        :system (my-git-explain--system)
        :stream t
        :callback (my-git-explain--callback buffer)))))

(defun my-git-explain--run (root entries marked intent &optional buffer)
  "Explain MARKED paths of ENTRIES at ROOT, with INTENT or nil.
Write into BUFFER (regenerate) or a new maximized explainer buffer."
  (let* ((default-directory root)
         (backend (my-git-explain--backend))
         (ctx (my-git-explain--context))
         (marked-list (if (eq marked 'all)
                          (my-git-explain--default-marks entries)
                        marked))
         (fit (my-git-explain--fit ctx entries marked-list)))
    (unless marked-list
      (user-error "Every changed file is skipped by default; use [s]elect"))
    (unless (car fit)
      (user-error "Change is %s over budget; use [s]elect"
                  (my-git-explain--size-label
                   (- (cdr fit) my-desktop-git-explain-budget))))
    (let ((buffer (or buffer
                      (get-buffer-create
                       (my-git-explain--buffer-name
                        (plist-get ctx :project) marked-list entries))))
          (fresh (not buffer)))
      (with-current-buffer buffer
        (when my-git-explain--active
          (gptel-abort buffer))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (my-git-explain-mode 1)
        (setq default-directory root)
        (setq my-git-explain--state
              (list :root root :entries entries :marked marked
                    :sent marked-list :intent intent :ctx ctx
                    :level (car fit) :size (cdr fit)
                    :backend (car backend) :model (cdr backend)
                    :started (float-time) :status 'running))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (my-git-explain--insert-header)
          (setq my-git-explain--state
                (plist-put my-git-explain--state :marker
                           (copy-marker (point) t))))
        (setq buffer-read-only t)
        (my-git-explain--header-line)
        (my-git-explain--send))
      (if fresh
          (my-git--maximize buffer)
        (pop-to-buffer-same-window buffer))
      (let ((skipped (seq-remove (lambda (e)
                                   (member (plist-get e :path) marked-list))
                                 entries)))
        (message "Explaining %d file%s with %s%s"
                 (length marked-list) (if (cdr marked-list) "s" "")
                 (cdr backend)
                 (if (and skipped (eq marked 'all))
                     (format "; skipped: %s (use [s]elect to include)"
                             (mapconcat (lambda (e) (plist-get e :path))
                                        skipped ", "))
                   ""))))))

;; ------------------------------------------------------------------
;; Finishing: context section, links, notes count
;; ------------------------------------------------------------------

(defun my-git-explain--known-files ()
  "Return a hash of existing repository paths (tracked and untracked)."
  (let ((known (make-hash-table :test #'equal)))
    (dolist (path (split-string
                   (my-git-explain--git "ls-files" "--cached" "--others"
                                        "--exclude-standard")
                   "\n" t))
      (when (file-exists-p path)
        (puthash path t known)))
    known))

(defun my-git-explain--resolve (name known)
  "Return the repository path NAME refers to in KNOWN, or nil.
A bare file name resolves when exactly one known path has it."
  (cond
   ((gethash name known) name)
   ((not (string-search "/" name))
    (let (found)
      (maphash (lambda (path _)
                 (when (equal (file-name-nondirectory path) name)
                   (push path found)))
               known)
      (and (= (length found) 1) (car found))))))

(defconst my-git-explain--cite-re
  (concat "\\([=~]?\\)"
          "\\([[:alnum:]_@+.-]+\\(?:/[[:alnum:]_@+.-]+\\)*\\)"
          ":\\([0-9]+\\)\\(-[0-9]+\\)?"
          "\\([=~]?\\)")
  "Regexp of a cited `path:line' (optionally a range, in =verbatim=).")

(defun my-git-explain--linkify (beg known)
  "Turn cited `path:line' after BEG into Org file links.
KNOWN is from `my-git-explain--known-files'."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward my-git-explain--cite-re nil t)
      (let* ((open (match-string 1))
             (close (match-string 5))
             (wrapped (and (not (string-empty-p open))
                           (equal open close)))
             (start (if wrapped (match-beginning 0) (match-beginning 2)))
             (end (if wrapped (match-end 0)
                    (or (match-end 4) (match-end 3))))
             (label (buffer-substring-no-properties
                     (match-beginning 2) (or (match-end 4) (match-end 3))))
             (line (match-string 3))
             (path (my-git-explain--resolve (match-string 2) known)))
        (when (and path
                   (not (save-match-data (org-in-src-block-p)))
                   (not (save-match-data
                          (org-in-regexp org-link-bracket-re 1))))
          (delete-region start end)
          (goto-char start)
          (insert (format "[[file:%s::%s][%s]]"
                          (expand-file-name path) line label)))))))

(defun my-git-explain--section (title)
  "Return (BEG . END) of the body of top-level section TITLE, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^\\* +%s[ \t]*$" (regexp-quote title)) nil t)
      (let ((beg (1+ (line-end-position))))
        (cons beg (if (re-search-forward "^\\* " nil t)
                      (line-beginning-position)
                    (point-max)))))))

(defun my-git-explain--add-diff-links (entries)
  "Append a [diff] link to Walkthrough headings naming one of ENTRIES."
  (when-let* ((section (my-git-explain--section "Walkthrough")))
    ;; Longest first: "src/a.py" must win over a changed "a.py".
    (let ((paths (sort (mapcar (lambda (e) (plist-get e :path)) entries)
                       (lambda (a b) (> (length a) (length b)))))
          (end (copy-marker (cdr section))))
      (save-excursion
        (goto-char (car section))
        (while (re-search-forward "^\\*\\* .*$" end t)
          (let* ((heading (match-string 0))
                 (path (seq-find
                        (lambda (p)
                          (or (string-search p heading)
                              (string-search
                               (format "[[file:%s::" (expand-file-name p))
                               heading)))
                        paths)))
            (when (and path (not (string-search "[[difft:" heading)))
              (end-of-line)
              (insert (format "  [[difft:%s][diff]]" path))))))
      (set-marker end nil))))

(defun my-git-explain--count-notes ()
  "Return the number of items under \"Worth a closer look\"."
  (if-let* ((section (my-git-explain--section "Worth a closer look")))
      (save-excursion
        (goto-char (car section))
        (let ((n 0))
          (while (re-search-forward "^[ \t]*[0-9]+[.)] " (cdr section) t)
            (setq n (1+ n)))
          n))
    0))

(defun my-git-explain--insert-context-section ()
  "Append the folded \"Context sent\" section."
  (let* ((st my-git-explain--state)
         (entries (plist-get st :entries))
         (sent (plist-get st :sent))
         (skipped (seq-remove (lambda (e) (member (plist-get e :path) sent))
                              entries))
         (instructions (plist-get (plist-get st :ctx) :instructions))
         heading)
    (goto-char (point-max))
    (insert (if (bolp) "\n" "\n\n"))
    (setq heading (point))
    (insert "* Context sent :noexport:\n"
            (format "- Request: %s of %s budget, %s\n"
                    (my-git-explain--size-label (plist-get st :size))
                    (my-git-explain--size-label
                     my-desktop-git-explain-budget)
                    (alist-get (plist-get st :level) my-git-explain--levels))
            (format "- Sent: %s\n" (string-join sent ", "))
            (if skipped
                (format "- Not shown: %s\n"
                        (mapconcat (lambda (e)
                                     (format "%s (%s)" (plist-get e :path)
                                             (or (plist-get e :skip)
                                                 "not selected")))
                                   skipped ", "))
              "")
            (format "- Instructions: %s\n"
                    (if instructions
                        (mapconcat #'car instructions ", ")
                      "none"))
            (format "- Intent: %s\n" (or (plist-get st :intent)
                                         "not stated")))
    (goto-char heading)
    (outline-hide-subtree)))

(defun my-git-explain--finish ()
  "Finish the explainer in the current buffer after a full response."
  (when my-git-explain--active
    (setq my-git-explain--active nil)
    (let* ((st my-git-explain--state)
           (inhibit-read-only t)
           (body-start (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^#\\+startup:.*\n" nil t)
                         (point))))
      (save-excursion
        (my-git-explain--linkify body-start
                                 (my-git-explain--known-files))
        (my-git-explain--add-diff-links (plist-get st :entries))
        (my-git-explain--insert-context-section))
      (set-buffer-modified-p nil)
      (my-git-explain--set-status
       (cons 'done (round (- (float-time) (plist-get st :started)))))
      (let ((notes (my-git-explain--count-notes)))
        (message "Explainer ready: %s — %d note%s worth a look"
                 (plist-get (plist-get st :ctx) :project)
                 notes (if (= notes 1) "" "s"))))))

;; ------------------------------------------------------------------
;; Explainer commands
;; ------------------------------------------------------------------

(defun my-git-explain-quit ()
  "Quit the explainer, restoring the layout it replaced.
A running request keeps streaming in the background."
  (interactive)
  (unless (my-git--restore-layout)
    (quit-window)))

(defun my-git-explain-cancel ()
  "Cancel the running request of this explainer."
  (interactive)
  (if my-git-explain--active
      (gptel-abort (current-buffer))
    (message "Nothing is running")))

(defun my-git-explain--next-marks (st entries)
  "Return marks for fresh ENTRIES after the explainer state ST.
Previously sent files stay marked, previously shown but unmarked
ones stay unmarked, new files get their default mark."
  (let ((marked (plist-get st :marked)))
    (if (eq marked 'all)
        'all
      (let ((known (mapcar (lambda (e) (plist-get e :path))
                           (plist-get st :entries)))
            (defaults (my-git-explain--default-marks entries)))
        (seq-filter (lambda (path)
                      (if (member path known)
                          (member path marked)
                        (member path defaults)))
                    (mapcar (lambda (e) (plist-get e :path)) entries))))))

(defun my-git-explain-regenerate (&optional reselect)
  "Collect the changes again and rewrite this explainer.
The previous selection is kept.  With RESELECT (\\[universal-argument]),
open the file checklist first."
  (interactive "P")
  (let* ((st my-git-explain--state)
         (buffer (current-buffer))
         (root (plist-get st :root))
         (intent (plist-get st :intent))
         (entries (let ((default-directory root))
                    (my-git-explain--collect)))
         (marks (my-git-explain--next-marks st entries)))
    (if reselect
        (my-git-explain--select
         root entries
         (if (eq marks 'all) (my-git-explain--default-marks entries) marks)
         (lambda (marked)
           (my-git-explain--run root entries marked intent buffer)))
      (my-git-explain--run root entries marks intent buffer))))

(defun my-git-explain--file-at-point ()
  "Return the path of the [diff]-linked heading around point, or nil."
  (save-excursion
    (let (path)
      (when (ignore-errors (org-back-to-heading t) t)
        (while (and (not path)
                    (progn
                      (when (looking-at ".*\\[\\[difft:\\([^]]+\\)\\]")
                        (setq path (match-string-no-properties 1)))
                      (and (not path) (org-up-heading-safe))))))
      path)))

(defun my-git-explain--open-diff (path)
  "Show the difftastic diff of changed PATH of this explainer's repo."
  (let ((default-directory (or (plist-get my-git-explain--state :root)
                               default-directory)))
    (if-let* ((change (assoc path (my-git--changed-files))))
        (apply #'my-git--diff-path change)
      (message "%s has no uncommitted changes now" path))))

(defun my-git-explain-diff ()
  "Show the difftastic diff of the file heading at point."
  (interactive)
  (my-git-explain--open-diff
   (or (my-git-explain--file-at-point)
       (user-error "No file heading at point"))))

(with-eval-after-load 'ol
  (org-link-set-parameters
   "difft"
   :follow (lambda (path _) (my-git-explain--open-diff path))))

;; ------------------------------------------------------------------
;; File checklist
;; ------------------------------------------------------------------

(defvar-local my-git-explain--select-state nil
  "Plist of the checklist: :root :entries :marked :ctx :callback.")

(defvar my-git-explain-select-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "m" #'my-git-explain-select-mark)
    (keymap-set map "u" #'my-git-explain-select-unmark)
    (keymap-set map "SPC" #'my-git-explain-select-toggle)
    (keymap-set map "t" #'my-git-explain-select-invert)
    (keymap-set map "U" #'my-git-explain-select-unmark-all)
    (keymap-set map "RET" #'my-git-explain-select-confirm)
    (keymap-set map "C-c C-c" #'my-git-explain-select-confirm)
    (keymap-set map "q" #'my-git-explain-select-quit)
    map)
  "Keymap of `my-git-explain-select-mode'.")

(define-derived-mode my-git-explain-select-mode tabulated-list-mode
  "Explain-Select"
  "Checklist of changed files to send to the explainer.
\\{my-git-explain-select-mode-map}"
  (setq tabulated-list-format [("" 1 nil)
                               ("Status" 22 t)
                               ("+/-" 12 nil :right-align t)
                               ("Size" 8 nil :right-align t)
                               ("File" 50 t)
                               ("Note" 0 nil)]
        tabulated-list-padding 1
        tabulated-list-use-header-line nil)
  (tabulated-list-init-header))

(defun my-git-explain--select-row (entry marked)
  "Return the tabulated-list row of ENTRY; MARKED is the marked paths."
  (let* ((path (plist-get entry :path))
         (on (member path marked))
         (face (unless on 'shadow))
         (cell (lambda (s) (if face (propertize s 'face face) s))))
    (list path
          (vector (if on "*" " ")
                  (funcall cell (my-git--status-label
                                 (plist-get entry :status)
                                 (plist-get entry :orig)))
                  (funcall cell
                           (cond ((plist-get entry :binary) "binary")
                                 ((plist-get entry :new)
                                  (format "+%d" (or (plist-get entry :added) 0)))
                                 (t (format "+%d −%d"
                                            (or (plist-get entry :added) 0)
                                            (or (plist-get entry :deleted) 0)))))
                  (funcall cell
                           (if (plist-get entry :binary) "—"
                             (my-git-explain--size-label
                              (alist-get 'full (plist-get entry :size)))))
                  (funcall cell path)
                  (funcall cell
                           (pcase (plist-get entry :skip)
                             ("binary" "binary (can't send)")
                             ('nil "")
                             (reason reason)))))))

(defun my-git-explain--select-summary ()
  "Update the checklist header line: marks and size vs budget."
  (let* ((st my-git-explain--select-state)
         (entries (plist-get st :entries))
         (marked (plist-get st :marked))
         (fit (my-git-explain--fit (plist-get st :ctx) entries marked))
         (budget my-desktop-git-explain-budget))
    (setq header-line-format
          (concat
           (format " Explain %s — %d of %d files marked · "
                   (plist-get (plist-get st :ctx) :project)
                   (length marked) (length entries))
           (if (car fit)
               (format "%s of %s budget (%s)"
                       (my-git-explain--size-label (cdr fit))
                       (my-git-explain--size-label budget)
                       (alist-get (car fit) my-git-explain--levels))
             (propertize (format "over budget by %s"
                                 (my-git-explain--size-label
                                  (- (cdr fit) budget)))
                         'face 'error))
           (propertize "   RET explain · m/u/SPC mark · t invert · U unmark all · q quit"
                       'face 'shadow)))
    (force-mode-line-update)
    fit))

(defun my-git-explain--select-refresh ()
  "Redraw the checklist rows and header."
  (let ((st my-git-explain--select-state))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (my-git-explain--select-row e (plist-get st :marked)))
                  (plist-get st :entries)))
    (tabulated-list-print t)
    (my-git-explain--select-summary)))

(defun my-git-explain--select (root entries marked callback)
  "Open the checklist of ENTRIES at ROOT with MARKED paths.
CALLBACK is called with the marked paths on confirmation."
  (let ((buffer (get-buffer-create "*explain: select files*"))
        (ctx (let ((default-directory root))
               (my-git-explain--context))))
    (with-current-buffer buffer
      (my-git-explain-select-mode)
      (setq default-directory root
            my-git-explain--select-state
            (list :root root :entries entries :marked marked
                  :ctx ctx :callback callback))
      (my-git-explain--select-refresh)
      (goto-char (point-min)))
    (my-git--maximize buffer)))

(defun my-git-explain--select-entry ()
  "Return the entry plist on the current checklist line, or nil."
  (when-let* ((path (tabulated-list-get-id)))
    (seq-find (lambda (e) (equal (plist-get e :path) path))
              (plist-get my-git-explain--select-state :entries))))

(defun my-git-explain--select-set (paths)
  "Set the marked PATHS of the checklist, keeping binaries out."
  (let ((st my-git-explain--select-state))
    (plist-put st :marked
               (seq-filter
                (lambda (path)
                  (and (member path paths)
                       (not (plist-get
                             (seq-find (lambda (e)
                                         (equal (plist-get e :path) path))
                                       (plist-get st :entries))
                             :binary))))
                (mapcar (lambda (e) (plist-get e :path))
                        (plist-get st :entries))))
    (my-git-explain--select-refresh)))

(defun my-git-explain--select-mark-line (state)
  "Set the current line's mark to STATE (t, nil or `toggle'), then move."
  (when-let* ((entry (my-git-explain--select-entry)))
    (let* ((path (plist-get entry :path))
           (marked (plist-get my-git-explain--select-state :marked))
           (on (if (eq state 'toggle) (not (member path marked)) state)))
      (if (and on (plist-get entry :binary))
          (message "Binary files can't be sent")
        (my-git-explain--select-set
         (if on (cons path marked) (remove path marked))))
      (forward-line 1))))

(defun my-git-explain-select-mark ()
  "Mark the file on this line."
  (interactive)
  (my-git-explain--select-mark-line t))

(defun my-git-explain-select-unmark ()
  "Unmark the file on this line."
  (interactive)
  (my-git-explain--select-mark-line nil))

(defun my-git-explain-select-toggle ()
  "Toggle the mark of the file on this line."
  (interactive)
  (my-git-explain--select-mark-line 'toggle))

(defun my-git-explain-select-invert ()
  "Invert all marks."
  (interactive)
  (let ((marked (plist-get my-git-explain--select-state :marked)))
    (my-git-explain--select-set
     (seq-remove (lambda (path) (member path marked))
                 (mapcar (lambda (e) (plist-get e :path))
                         (plist-get my-git-explain--select-state
                                    :entries))))))

(defun my-git-explain-select-unmark-all ()
  "Unmark all files."
  (interactive)
  (my-git-explain--select-set nil))

(defun my-git-explain-select-quit ()
  "Close the checklist without explaining."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (my-git--restore-layout)
      (quit-window))
    (kill-buffer buffer)))

(defun my-git-explain-select-confirm ()
  "Explain the marked files."
  (interactive)
  (let* ((st my-git-explain--select-state)
         (marked (plist-get st :marked))
         (fit (my-git-explain--select-summary)))
    (cond
     ((null marked) (message "Mark at least one file"))
     ((not (car fit))
      (message "Over budget by %s; unmark something"
               (my-git-explain--size-label
                (- (cdr fit) my-desktop-git-explain-budget))))
     (t
      ;; Restore the layout under the checklist first: the explainer
      ;; then remembers that layout for its own `q'.
      (let ((callback (plist-get st :callback))
            (paths (seq-filter (lambda (p) (member p marked))
                               (mapcar (lambda (e) (plist-get e :path))
                                       (plist-get st :entries)))))
        (my-git-explain-select-quit)
        (funcall callback paths))))))

;; ------------------------------------------------------------------
;; Entry point
;; ------------------------------------------------------------------

(defun my-git-explain (&optional ask-intent)
  "Explain the uncommitted changes of the current project with an LLM.
Asks for [a]ll changes (files skipped by default left out) or
[s]elect files in a checklist.  With ASK-INTENT
\(\\[universal-argument]), also ask what the change was supposed to do."
  (interactive "P")
  (let* ((root (my-git--root))
         (default-directory root))
    (my-git-explain--backend)
    (let* ((choice (car (read-multiple-choice
                         "Explain: "
                         '((?a "all changes")
                           (?s "select files")
                           (?q "quit")))))
           (entries (unless (eq choice ?q) (my-git-explain--collect)))
           (intent-fn (lambda ()
                        (when ask-intent
                          (let ((s (string-trim
                                    (read-string "What was this change supposed to do? "))))
                            (unless (string-empty-p s) s))))))
      (pcase choice
        (?a (my-git-explain--run root entries 'all (funcall intent-fn)))
        (?s (my-git-explain--select
             root entries (my-git-explain--default-marks entries)
             (lambda (marked)
               (my-git-explain--run root entries marked
                                    (funcall intent-fn)))))))))

(provide 'desktop-git-explain)
;;; desktop-git-explain.el ends here
