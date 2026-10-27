;;; desktop-project.el --- Project workspaces for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in project.el projects mapped to perspective.el workspaces.
;; Opening a project creates its perspective on demand and initializes
;; it once with a ghostel terminal on the left and Magit on the right.
;;
;; C-c g: Magit status plus difftastic (difft) views of the uncommitted
;; changes -- staged, unstaged, both, or one picked changed file.  The
;; diff opens maximized over the whole frame, side windows included
;; (side-by-side needs the width); `q' restores the layout it replaced.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function my-term-new "desktop-term" (&optional dir))
(declare-function persp-add-buffer "perspective" (buffer-or-name))
(declare-function persp-current-name "perspective")
(declare-function persp-names "perspective")
(declare-function persp-switch "perspective" (name &optional norecord))

(defvar magit-display-buffer-function)
(defvar magit-display-buffer-noselect)

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status-setup-buffer magit-toplevel))

;;; Commit safety

(defvar git-commit-mode)
(defvar magit-commit-show-diff)
(defvar transient-values)
(declare-function magit-commit-diff "magit-commit" ())

(defun my-desktop-magit--large-diff-p ()
  "Return non-nil when the staged diff exceeds the configured size.
The diff is piped through `head', so at most one character more than
the limit is read into Emacs."
  (when my-desktop-magit-max-diff-size
    (let* ((limit my-desktop-magit-max-diff-size)
           (default-directory (or (magit-toplevel) default-directory))
           (command (format (concat "git diff --cached --no-color"
                                    " --no-ext-diff | head -c %d")
                            (1+ limit))))
      (with-temp-buffer
        (call-process-shell-command command nil t)
        (> (buffer-size) limit)))))

(defun my-desktop-magit--trim-large-inline-diff ()
  "Remove an oversized inline diff from a commit message buffer.
Git appends the staged diff below its cut line when `git commit
--verbose' is used.  Magit then fontifies the whole diff in
`git-commit-propertize-diff'; for tens of megabytes that blocks
Emacs.  This runs first in `git-commit-setup-hook', before that
fontification, and leaves the message above the cut line intact."
  (when (and my-desktop-magit-max-diff-size
             (bound-and-true-p git-commit-mode))
    (let ((comment (or comment-start "#")))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format "^%s -\\{8,\\} >8 -\\{8,\\}$"
                       (regexp-quote comment))
               nil t)
          (let ((start (line-beginning-position))
                (size (- (point-max) (line-beginning-position))))
            (when (> size my-desktop-magit-max-diff-size)
              (delete-region start (point-max))
              (insert (format (concat "%s Inline diff omitted (%.1f MiB)"
                                      "; use C-c C-d to show it.\n")
                              comment
                              (/ size (* 1024.0 1024.0)))))))))))

(defun my-desktop-magit--commit-diff-around (orig)
  "Skip the automatic commit diff buffer for a huge staged diff.
ORIG is `magit-commit-diff'."
  (if (and (bound-and-true-p git-commit-mode)
           (my-desktop-magit--large-diff-p))
      (let ((magit-commit-show-diff nil))
        (message (concat "Staged diff is large; automatic diff"
                         " suppressed (C-c C-d to show)"))
        (funcall orig))
    (funcall orig)))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook
            #'my-desktop-magit--trim-large-inline-diff))

(with-eval-after-load 'magit-commit
  (unless my-desktop-magit-inline-diff
    (setf (alist-get 'magit-commit transient-values) nil))
  (advice-add 'magit-commit-diff :around
              #'my-desktop-magit--commit-diff-around))

(defvar my-project--root-to-perspective (make-hash-table :test #'equal)
  "Map canonical project roots to perspective names for this session.")

(defvar my-project--perspective-to-root (make-hash-table :test #'equal)
  "Map project perspective names to canonical roots for this session.")

(defvar my-project--initialized (make-hash-table :test #'equal)
  "Set of project roots whose perspective layout was initialized.")

(defvar my-project--renaming nil
  "Root and old name of the project perspective currently being renamed.")

(defun my-project--canonical-root (root)
  "Return ROOT as a canonical local directory name.
Signal a user error when ROOT is remote or is not an existing directory."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (when (file-remote-p root)
      (user-error "Remote project workspaces are not supported: %s" root))
    (unless (file-directory-p root)
      (user-error "Project directory does not exist: %s" root))
    (file-name-as-directory (file-truename root))))

(defun my-project--basename (root)
  "Return a non-empty display name for project ROOT."
  (let ((name (file-name-nondirectory (directory-file-name root))))
    (if (string-empty-p name) "project" name)))

(defun my-project--available-name-p (name)
  "Return non-nil when perspective NAME is currently unused."
  (and (not (member name (persp-names)))
       (not (gethash name my-project--perspective-to-root))))

(defun my-project--perspective-name (root)
  "Return an unused, readable perspective name for project ROOT."
  (let* ((base (my-project--basename root))
         (parent-dir (file-name-directory (directory-file-name root)))
         (parent (and parent-dir
                      (file-name-nondirectory
                       (directory-file-name parent-dir))))
         (qualified (and parent (not (string-empty-p parent))
                         (format "%s/%s" parent base))))
    (cond
     ((my-project--available-name-p base) base)
     ((and qualified (my-project--available-name-p qualified)) qualified)
     (t
      (let* ((short-hash (substring (secure-hash 'sha1 root) 0 8))
             (stem (format "%s [%s]" base short-hash))
             (name stem)
             (suffix 2))
        (while (not (my-project--available-name-p name))
          (setq name (format "%s-%d" stem suffix)
                suffix (1+ suffix)))
        name)))))

(defun my-project--dissociate-root (root)
  "Remove the project association for ROOT."
  (when-let ((name (gethash root my-project--root-to-perspective)))
    (remhash root my-project--root-to-perspective)
    (when (equal (gethash name my-project--perspective-to-root) root)
      (remhash name my-project--perspective-to-root))
    (remhash root my-project--initialized)))

(defun my-project--dissociate-name (name)
  "Remove the project association for perspective NAME."
  (when-let ((root (gethash name my-project--perspective-to-root)))
    (remhash name my-project--perspective-to-root)
    (when (equal (gethash root my-project--root-to-perspective) name)
      (remhash root my-project--root-to-perspective))
    (remhash root my-project--initialized)))

(defun my-project--associate (root name)
  "Associate canonical project ROOT with perspective NAME.
Repair any stale association occupying either side of the mapping first."
  (let ((initialized (gethash root my-project--initialized)))
    (my-project--dissociate-root root)
    (my-project--dissociate-name name)
    (puthash root name my-project--root-to-perspective)
    (puthash name root my-project--perspective-to-root)
    (when initialized
      (puthash root t my-project--initialized))))

(defun my-project--prune-stale-associations ()
  "Remove associations whose perspectives are no longer live."
  (let ((live (persp-names)) stale)
    (maphash (lambda (name _root)
               (unless (member name live)
                 (push name stale)))
             my-project--perspective-to-root)
    (dolist (name stale)
      (my-project--dissociate-name name))))

(defun my-project-current-root ()
  "Return the project root associated with the current perspective.
Return nil when the current perspective is a normal, non-project workspace."
  (when (bound-and-true-p persp-mode)
    (gethash (persp-current-name) my-project--perspective-to-root)))

(defun my-project--before-perspective-rename ()
  "Remember a project perspective association before it is renamed."
  (let* ((old-name (persp-current-name))
         (root (gethash old-name my-project--perspective-to-root)))
    (setq my-project--renaming (and root (cons root old-name)))))

(defun my-project--after-perspective-rename ()
  "Update the project association after a perspective rename."
  (when my-project--renaming
    (pcase-let ((`(,root . ,old-name) my-project--renaming))
      (remhash old-name my-project--perspective-to-root)
      (my-project--associate root (persp-current-name))))
  (setq my-project--renaming nil))

(defun my-project--perspective-killed ()
  "Forget the project associated with the perspective being killed."
  (my-project--dissociate-name (persp-current-name)))

(with-eval-after-load 'perspective
  (add-hook 'persp-before-rename-hook
            #'my-project--before-perspective-rename)
  (add-hook 'persp-after-rename-hook
            #'my-project--after-perspective-rename)
  (add-hook 'persp-killed-hook #'my-project--perspective-killed))

(defun my-project--assert-git-root (root)
  "Signal a user error unless ROOT is the top of a local Git repository."
  (require 'magit)
  (let ((git-root (magit-toplevel root)))
    (unless (and git-root (file-equal-p root git-root))
      (user-error "Project is not a Git repository root: %s" root))))

(defun my-project--main-window ()
  "Return a normal window suitable for the project layout."
  (or (and (not (window-parameter (selected-window) 'window-side))
           (selected-window))
      (seq-find (lambda (window)
                  (not (window-parameter window 'window-side)))
                (window-list))
      (selected-window)))

(defun my-project--initialize-layout (root)
  "Initialize the current perspective's two-pane layout for ROOT."
  (unless (fboundp 'my-term-new)
    (user-error "The ghostel terminal module is disabled"))
  (require 'magit)
  (select-window (my-project--main-window))
  (delete-other-windows (selected-window))
  (my-term-new root)
  (let* ((left (selected-window))
         (term-buffer (window-buffer left)))
    ;; Ghostel normally reuses the selected window.  Collapse any extra
    ;; normal windows it created before constructing the requested layout.
    (delete-other-windows left)
    (let ((right (split-window-right)))
      (let ((magit-display-buffer-noselect t)
            (magit-display-buffer-function
             (lambda (buffer)
               (set-window-buffer right buffer)
               right)))
        (with-selected-window right
          (let ((magit-buffer (magit-status-setup-buffer root)))
            (set-window-buffer right magit-buffer)
            (when (bound-and-true-p persp-mode)
              (persp-add-buffer magit-buffer)))))
      (set-window-buffer left term-buffer)
      (when (bound-and-true-p persp-mode)
        (persp-add-buffer term-buffer))
      (select-window left))))

(defun my-project--known-project-alist ()
  "Return completion candidates for known project roots."
  (mapcar
   (lambda (root)
     (let* ((root (expand-file-name root))
            (label (format "%s  —  %s"
                           (my-project--basename root)
                           (abbreviate-file-name root))))
       (cons label root)))
   (project-known-project-roots)))

(defun my-project-read-root ()
  "Prompt for and return one of the roots known to project.el."
  (let ((projects (my-project--known-project-alist)))
    (unless projects
      (user-error "No known projects; add one with C-c b a"))
    (let ((choice (completing-read "Open project: " projects nil t)))
      (my-project--canonical-root (cdr (assoc choice projects))))))

(defun my-project-open (&optional root)
  "Open project ROOT in its associated perspective.
Interactively, select ROOT from the projects known to project.el.  A new
project perspective is initialized once with Ghostel on the left and Magit
status on the right.  An existing perspective keeps its current layout."
  (interactive)
  (setq root (my-project--canonical-root (or root (my-project-read-root))))
  (my-project--assert-git-root root)
  (my-project--prune-stale-associations)
  (let ((name (gethash root my-project--root-to-perspective)))
    ;; Both tables must agree.  Repair a partial or overwritten mapping.
    (when (and name
               (not (equal (gethash name my-project--perspective-to-root)
                           root)))
      (my-project--dissociate-root root)
      (setq name nil))
    (unless name
      (setq name (my-project--perspective-name root))
      (my-project--associate root name))
    (persp-switch name)
    (unless (gethash root my-project--initialized)
      (my-project--initialize-layout root)
      (puthash root t my-project--initialized))))

(defun my-project-add (directory)
  "Add the Git project containing DIRECTORY to project.el's known projects."
  (interactive (list (read-directory-name "Add project directory: "
                                          default-directory nil t)))
  (let* ((directory (my-project--canonical-root directory))
         (project (project-current nil directory)))
    (unless project
      (user-error "No project recognized in: %s" directory))
    (let ((root (my-project--canonical-root (project-root project))))
      (my-project--assert-git-root root)
      (project-remember-project project)
      (message "Added project: %s" (abbreviate-file-name root)))))

(defun my-project-terminal ()
  "Open a new terminal at the current perspective's project root.
In a normal workspace, retain `my-term-new' fallback directory behavior."
  (interactive)
  (unless (fboundp 'my-term-new)
    (user-error "The ghostel terminal module is disabled"))
  (my-term-new (my-project-current-root)))

;;; Git status and difftastic diffs (C-c g)

(defvar difftastic-executable)
(defvar difftastic-mode-map)
(defvar difftastic-display-buffer-function)
(defvar difftastic-requested-window-width-function)
(declare-function difftastic-git-diff-range "difftastic"
                  (&optional rev-or-range args files))
(declare-function difftastic-files "difftastic"
                  (file-A file-B &optional lang-override))
(declare-function difftastic-quit "difftastic" ())
(declare-function persp-current-name "perspective")

(use-package difftastic
  :ensure t
  :defer t
  :commands (difftastic-git-diff-range difftastic-files)
  :config
  ;; Every difftastic view opens maximized; `q' restores the layout.
  ;; Set globally: difftastic reads the display function in its process
  ;; sentinel, after any `let' around the call has already ended.
  (setq difftastic-executable my-desktop-difftastic-executable
        difftastic-display-buffer-function #'my-git--diff-display
        difftastic-requested-window-width-function #'my-git--diff-width)
  (keymap-set difftastic-mode-map "q" #'my-git-diff-quit))

(defvar-local my-git--restore nil
  "Window layout to restore when this maximized buffer is quit.
A cons (PERSPECTIVE . WINDOW-CONFIGURATION).  Permanent: difftastic
re-enables its major mode on every run and rerun.")
(put 'my-git--restore 'permanent-local t)

(defun my-git--root ()
  "Return the Git root for the C-c g commands.
The project perspective's root wins; otherwise the repository of
`default-directory'."
  (require 'magit)
  (or (my-project-current-root)
      (magit-toplevel)
      (user-error "Not in a Git repository")))

(defun my-git--persp-name ()
  "Return the current perspective name, or nil."
  (and (bound-and-true-p persp-mode) (persp-current-name)))

(defun my-git--diff-width ()
  "Return the difft width for a diff shown maximized in the frame.
The selected window's fringe/margin overhead is subtracted."
  (let ((window (my-project--main-window)))
    (- (window-total-width (frame-root-window))
       (- (window-total-width window)
          (window-max-chars-per-line window)))))

(defun my-git--maximize (buffer)
  "Show BUFFER maximized, over side windows too.
The previous layout (treemacs included) is remembered in BUFFER and
restored by `my-git--restore-layout'."
  (let ((restore (cons (my-git--persp-name)
                       (current-window-configuration))))
    (select-window (my-project--main-window))
    ;; Ignoring window parameters lets the side windows go as well.
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (set-window-dedicated-p nil nil)
    (switch-to-buffer buffer nil t)
    (with-current-buffer buffer
      (setq my-git--restore restore))))

(defun my-git--diff-display (buffer _requested-width)
  "Show difftastic BUFFER maximized; see `my-git--maximize'."
  (my-git--maximize buffer))

(defun my-git--restore-layout ()
  "Bury the current buffer and restore the layout it replaced.
Return nil, doing nothing, when there is no layout to restore or the
perspective changed since."
  (let ((restore my-git--restore))
    (when (and restore (equal (car restore) (my-git--persp-name)))
      (setq my-git--restore nil)
      (bury-buffer)
      (set-window-configuration (cdr restore))
      t)))

(defun my-git-diff-quit ()
  "Quit a difftastic buffer, restoring the layout it replaced.
Falls back to `difftastic-quit' when there is nothing to restore
or the perspective changed since."
  (interactive)
  (unless (my-git--restore-layout)
    (difftastic-quit)))

(defun my-git--check-difft ()
  "Signal a user error unless the difft executable is available."
  (unless (executable-find my-desktop-difftastic-executable)
    (user-error "difft (%s) not found in PATH"
                my-desktop-difftastic-executable))
  (require 'difftastic))

(defun my-git--changes-p (&rest args)
  "Return non-nil when `git diff ARGS' is not empty."
  (/= 0 (apply #'process-file "git" nil nil nil
               "diff" "--quiet" "--no-ext-diff" args)))

(defmacro my-git--without-prefix (&rest body)
  "Run BODY with no prefix argument.
difftastic commands read `current-prefix-arg' (C-u C-u asks for
extra difft arguments); the C-c g commands do not offer that."
  (declare (indent 0) (debug t))
  `(let ((current-prefix-arg nil))
     ,@body))

(defun my-git--diff (label rev args)
  "Show `git diff ARGS REV' with difftastic; LABEL names the empty case."
  (my-git--check-difft)
  (let ((default-directory (my-git--root)))
    (if (not (apply #'my-git--changes-p
                    (append args (and rev (list rev)))))
        (message "No %s changes" label)
      (my-git--without-prefix
        (difftastic-git-diff-range rev args)))))

(defun my-git-status ()
  "Open Magit status for the current project (see `my-git--root')."
  (interactive)
  (require 'magit)
  (magit-status-setup-buffer (my-git--root)))

(defun my-git-diff-staged ()
  "Show staged changes (index vs HEAD) with difftastic."
  (interactive)
  (my-git--diff "staged" nil '("--cached")))

(defun my-git-diff-unstaged ()
  "Show unstaged changes (worktree vs index) with difftastic.
Untracked files are not included; see `my-git-diff-file'."
  (interactive)
  (my-git--diff "unstaged" nil nil))

(defun my-git-diff-all ()
  "Show staged and unstaged changes (worktree vs HEAD) with difftastic.
Untracked files are not included; see `my-git-diff-file'."
  (interactive)
  (my-git--diff "uncommitted" "HEAD" nil))

(defun my-git--changed-files ()
  "Return changed files of the repository at `default-directory'.
Each element is (PATH STATUS ORIG): STATUS is the two-letter
porcelain code, ORIG the source path of a rename or copy."
  (let ((fields (split-string
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil t nil "status"
                                   "--porcelain=v1" "-z"
                                   "--untracked-files=all")))
                 "\0" t))
        files)
    (while fields
      (let* ((entry (pop fields))
             (status (substring entry 0 2))
             (path (substring entry 3))
             (orig (and (memq (aref status 0) '(?R ?C)) (pop fields))))
        (push (list path status orig) files)))
    (nreverse files)))

(defun my-git--status-label (status orig)
  "Describe porcelain STATUS (and rename source ORIG) for the picker."
  (let ((x (aref status 0))
        (y (aref status 1)))
    (cond
     ((equal status "??") "untracked")
     ((or (eq x ?U) (eq y ?U) (member status '("AA" "DD"))) "conflict")
     (t
      (string-join
       (delq nil
             (list (pcase x
                     ((or ?M ?T) "staged")
                     (?A "added")
                     (?D "deleted")
                     (?R (format "renamed from %s" orig))
                     (?C (format "copied from %s" orig)))
                   (pcase y
                     ((or ?M ?T) "unstaged")
                     (?A "intent to add")
                     (?D "deleted in worktree"))))
       ", ")))))

(defun my-git-diff-file ()
  "Pick a changed file and show its diff with difftastic.
Staged, unstaged and untracked files are offered.  A tracked file
is shown as worktree vs HEAD; an untracked one as a new file."
  (interactive)
  (my-git--check-difft)
  (let* ((default-directory (my-git--root))
         (files (or (my-git--changed-files)
                    (user-error "No changed files")))
         (annotate
          (lambda (path)
            (pcase-let ((`(,_ ,status ,orig) (assoc path files)))
              (concat "  " (propertize (my-git--status-label status orig)
                                       'face 'completions-annotations)))))
         (path (let ((completion-extra-properties
                      (list :annotation-function annotate)))
                 (completing-read "Diff file: " (mapcar #'car files)
                                  nil t))))
    (apply #'my-git--diff-path (assoc path files))))

(defun my-git--diff-path (path status orig)
  "Show the difftastic diff of changed PATH in `default-directory'.
STATUS is its porcelain code and ORIG a rename source, as returned
by `my-git--changed-files'.  A tracked file is shown as worktree vs
HEAD; an untracked one as a new file."
  (my-git--check-difft)
  (my-git--without-prefix
    (if (equal status "??")
        (difftastic-files null-device (expand-file-name path))
      (difftastic-git-diff-range
       "HEAD" nil (delq nil (list path orig))))))

(provide 'desktop-project)
;;; desktop-project.el ends here
