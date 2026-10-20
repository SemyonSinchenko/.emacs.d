;;; desktop-project.el --- Project workspaces for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in project.el projects mapped to perspective.el workspaces.
;; Opening a project creates its perspective on demand and initializes
;; it once with a ghostel terminal on the left and Magit on the right.

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

(provide 'desktop-project)
;;; desktop-project.el ends here
