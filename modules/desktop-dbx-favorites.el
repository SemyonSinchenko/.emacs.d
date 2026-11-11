;;; desktop-dbx-favorites.el --- Favorite Databricks jobs for dbx-el -*- lexical-binding: t; -*-

;;; Commentary:
;; The Databricks UI "favorite" star, for dbx-el.
;;
;; Favorites are per project, like the dbx contexts / profiles they
;; point to: an Org file `.dbx-el.org'
;; (`my-desktop-dbx-favorites-file-name') found upwards from
;; `default-directory'.  It is created at the project root (project.el;
;; asked for outside a project) on the first star; without one the
;; project simply has no favorites.  One heading per job; the heading
;; is the shown name, the body is free notes:
;;
;;   * Nightly ingest
;;   :PROPERTIES:
;;   :DBX_JOB_ID: 123456789
;;   :DBX_CONTEXT: prod
;;   :END:
;;
;; The context is the dbx context the job was starred in, so one
;; dashboard can show jobs of several workspaces.
;;
;; - `my-dbx-favorites' (C-c d f): this project's favorites with their
;;   latest runs (glyphs oldest -> newest), last start, duration and
;;   status; it refreshes itself (fast while something runs).  RET
;;   opens the job page in the favorite's context, a adds, k removes,
;;   e edits the Org file, o browser, w/W copy id/URL, / filter.
;; - `my-dbx-favorite-toggle' (* in job pages, run pages and the jobs
;;   list): star / unstar the job at point.
;;
;; Built on dbx-el's public API (README "Extending"); loaded lazily
;; after dbx-jobs (see desktop-dbx.el).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'project)
(require 'dbx-context)
(require 'dbx-page)
(require 'dbx-jobs)
(require 'desktop-config-defs)

(defvar dbx-page-refresh-active)

;;;; Helpers

(defun my-dbx--open-job (id &optional context)
  "Open the dbx page of job ID in CONTEXT (nil: the active one).
The page also gets the caller's directory, so * there stars into
the same project's favorites."
  (let ((dir default-directory)
        (buf (dbx-job-page id context)))
    (with-current-buffer buf (setq default-directory dir))
    buf))

;;;; Storage (Org file)

(defun my-dbx--root ()
  "Directory whose favorites apply to `default-directory'.
The directory of the nearest favorites file upwards, else the
project root, else nil."
  (let ((dir (locate-dominating-file default-directory
                                     my-desktop-dbx-favorites-file-name)))
    (cond (dir (file-name-as-directory (expand-file-name dir)))
          ((project-current)
           (file-name-as-directory
            (expand-file-name (project-root (project-current))))))))

(defun my-dbx--favorites-file (&optional create)
  "The favorites file for `default-directory', or nil if there is none.
With CREATE, return where it would be created: the project root,
or a directory asked for outside a project."
  (let* ((root (my-dbx--root))
         (file (and root (expand-file-name my-desktop-dbx-favorites-file-name
                                           root))))
    (cond ((and file (file-exists-p file)) file)
          (create
           (expand-file-name
            my-desktop-dbx-favorites-file-name
            (or root (read-directory-name
                      "Not in a project; favorites file in: "
                      default-directory nil t)))))))

(defun my-dbx--favorites-buffer (&optional create)
  "Buffer visiting the favorites file, or nil if there is none.
With CREATE, create the file first (see `my-dbx--favorites-file')."
  (when-let* ((file (my-dbx--favorites-file create)))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+title: Databricks favorites\n"
                "# One heading per job (the heading is the shown name).\n"
                "# Properties: DBX_JOB_ID, DBX_CONTEXT (dbx context name).\n\n"))
      (message "dbx: created %s" (abbreviate-file-name file)))
    (find-file-noselect file)))

(defun my-dbx--favorites ()
  "Favorites as plists (:id :context :name :pos :file), in file order.
nil when the project has no favorites file."
  (when-let* ((buf (my-dbx--favorites-buffer)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (list :id (string-trim (org-entry-get nil "DBX_JOB_ID"))
                :context (let ((c (org-entry-get nil "DBX_CONTEXT")))
                           (and c (string-trim c)))
                :name (org-get-heading t t t t)
                :pos (point)
                :file buffer-file-name))
        "DBX_JOB_ID={.}" 'file)))))

(defun my-dbx--favorite-find (id context)
  "The favorite with job ID in CONTEXT, or nil."
  (cl-find-if (lambda (f) (and (equal (plist-get f :id) (format "%s" id))
                               (equal (plist-get f :context) context)))
              (my-dbx--favorites)))

(defun my-dbx--favorite-add (id context name)
  "Store job ID of CONTEXT as a favorite called NAME.
Creates the project's favorites file on the first star."
  (with-current-buffer (my-dbx--favorites-buffer t)
    (org-with-wide-buffer
     (goto-char (point-max))
     (unless (bolp) (insert "\n"))
     (insert (format "* %s\n:PROPERTIES:\n:DBX_JOB_ID: %s\n:DBX_CONTEXT: %s\n:END:\n"
                     name id context)))
    (save-buffer))
  (my-dbx--favorites-changed))

(defun my-dbx--favorite-remove (fav)
  "Delete favorite FAV (a plist from `my-dbx--favorites')."
  (with-current-buffer (find-file-noselect (plist-get fav :file))
    (org-with-wide-buffer
     (goto-char (plist-get fav :pos))
     (delete-region (point) (org-end-of-subtree t t)))
    (save-buffer))
  (my-dbx--favorites-changed))

(defun my-dbx--favorites-changed ()
  "Refresh the open favorites dashboards of this project."
  (let ((root (my-dbx--root)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'my-dbx-favorites-mode)
                   (equal (my-dbx--root) root))
          (dbx-page-refresh))))))

(defun my-dbx--job-name (id context)
  "Fetch the name of job ID in CONTEXT (blocking)."
  (or (ignore-errors
        (dbx-job-name
         (dbx-await (lambda (k e)
                      (dbx-api "get" "/api/2.2/jobs/get"
                               :params `((job_id . ,id)) :context context
                               :callback k :error-callback e))
                    "job")))
      (format "Job %s" id)))

;;;###autoload
(defun my-dbx-favorite-toggle (id &optional name context)
  "Star or unstar job ID of CONTEXT (default: the active context).
Interactively the job at point (job / run page, jobs and runs
lists), else a job ID or URL is asked for.  NAME defaults to the
job's name."
  (interactive
   (let ((job (or (dbx-job-at-point)
                  (let ((ref (dbx-read-object-ref "Job ID or URL" '(job run))))
                    (list :id (or (plist-get ref :job-id) (plist-get ref :id))
                          :context (plist-get ref :context))))))
     (list (plist-get job :id) (plist-get job :name) (plist-get job :context))))
  (let* ((id (format "%s" id))
         (context (dbx-context-name context))
         (fav (my-dbx--favorite-find id context)))
    (if fav
        (progn (my-dbx--favorite-remove fav)
               (message "dbx: unstarred %s (%s) in %s" (plist-get fav :name)
                        context (abbreviate-file-name (plist-get fav :file))))
      (let ((name (or name (my-dbx--job-name id context))))
        (my-dbx--favorite-add id context name)
        (message "dbx: starred %s (%s) in %s" name context
                 (abbreviate-file-name (my-dbx--favorites-file)))))))

;;;; Dashboard

(defvar-local my-dbx-favorites--results nil
  "Hash (ID . CONTEXT) -> (:runs RUNS) or (:error MESSAGE).")
(defvar-local my-dbx-favorites--gen 0
  "Refresh generation; answers of older refreshes are dropped.")
(defvar-local my-dbx-favorites--favs nil)

;; Row ids are (ID . CONTEXT): dbx-list-mode calls the open / URL
;; functions with ID while the row's CONTEXT is active.
(define-derived-mode my-dbx-favorites-mode dbx-list-mode "DBX-Favorites"
  "Favorite Databricks jobs with their latest runs.

\\{my-dbx-favorites-mode-map}"
  (setq tabulated-list-format
        [("Last runs" 10 nil) ("Name" 36 t) ("Context" 10 t) ("Job ID" 18 t)
         ("Last run" 10 nil) ("Duration" 10 nil :right-align t) ("Status" 14 t)])
  (setq tabulated-list-sort-key nil)    ; keep the Org file order
  (setq-local dbx-list-open-function #'my-dbx--open-job)
  (setq-local dbx-list-url-function
              (lambda (id) (dbx-url (format "/jobs/%s" id))))
  (setq-local dbx-page-refresh-function #'my-dbx-favorites--refresh)
  (setq-local dbx-page-refresh-active my-desktop-dbx-favorites-refresh)
  (setq-local dbx-page-actions
              '(("RET" "Open job" dbx-page-open)
                ("a" "Add a favorite (ID or URL)" my-dbx-favorite-toggle)
                ("k" "Remove this favorite" my-dbx-favorites-remove)
                ("e" "Edit the favorites file" my-dbx-favorites-edit)
                ("/" "Filter" dbx-list-filter)))
  (setq my-dbx-favorites--results (make-hash-table :test #'equal))
  (tabulated-list-init-header))

(keymap-set my-dbx-favorites-mode-map "a" #'my-dbx-favorite-toggle)
(keymap-set my-dbx-favorites-mode-map "k" #'my-dbx-favorites-remove)
(keymap-set my-dbx-favorites-mode-map "e" #'my-dbx-favorites-edit)

;;;###autoload
(defun my-dbx-favorites ()
  "Show this project's favorite jobs with their latest runs.
The project is the one of `default-directory' (one dashboard per
project); without a favorites file the list is empty."
  (interactive)
  (let* ((root (or (my-dbx--root)
                   (file-name-as-directory (expand-file-name default-directory))))
         (buf (get-buffer-create
               (format "*dbx favorites: %s*"
                       (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buf
      (setq default-directory root)
      (unless (derived-mode-p 'my-dbx-favorites-mode)
        (my-dbx-favorites-mode))
      (my-dbx-favorites--refresh))
    (pop-to-buffer buf)))

(defun my-dbx-favorites-remove ()
  "Remove the favorite at point."
  (interactive)
  (let* ((key (or (tabulated-list-get-id) (user-error "No favorite here")))
         (fav (my-dbx--favorite-find (car key) (cdr key))))
    (when (and fav (y-or-n-p (format "Unstar %s? " (plist-get fav :name))))
      (my-dbx--favorite-remove fav))))

(defun my-dbx-favorites-edit ()
  "Open the project's favorites Org file (created if missing)."
  (interactive)
  (pop-to-buffer (my-dbx--favorites-buffer t)))

(defun my-dbx-favorites--entry (fav)
  "Tabulated-list entry of FAV from the latest results."
  (let* ((key (cons (plist-get fav :id) (plist-get fav :context)))
         (res (gethash key my-dbx-favorites--results))
         (err (plist-get res :error))
         (runs (plist-get res :runs))
         (last-run (car runs)))          ; runs/list is newest first
    (list key
          (vector (dbx-runs-glyph-strip runs)
                  (or (plist-get fav :name) "")
                  (or (plist-get fav :context) "")
                  (plist-get fav :id)
                  (if last-run (dbx-format-ago (alist-get 'start_time last-run)) "")
                  (if last-run (dbx-format-duration (dbx-run-duration last-run)) "")
                  (cond (err (propertize "error" 'face 'dbx-failed 'help-echo err))
                        ((null res) (propertize "…" 'face 'dbx-dim))
                        (last-run (dbx-run-status-label last-run))
                        (t (propertize "no runs" 'face 'dbx-dim)))))))

(defun my-dbx-favorites--render ()
  "Re-render the dashboard from `my-dbx-favorites--results'."
  (dbx-list-set-entries (mapcar #'my-dbx-favorites--entry my-dbx-favorites--favs)))

(defun my-dbx-favorites--refresh ()
  "Re-read the favorites and fetch their latest runs."
  (let* ((buf (current-buffer))
         (gen (cl-incf my-dbx-favorites--gen))
         (favs (my-dbx--favorites))
         (pending (length favs))
         (done
          (lambda (key result)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (when (= gen my-dbx-favorites--gen)
                  (puthash key result my-dbx-favorites--results)
                  (my-dbx-favorites--render)
                  (when (zerop (cl-decf pending))
                    (setq dbx-page-active-p
                          (cl-loop for v being the hash-values
                                   of my-dbx-favorites--results
                                   thereis (cl-some #'dbx-run-active-p
                                                    (plist-get v :runs))))
                    (dbx-page-schedule-refresh))))))))
    (setq my-dbx-favorites--favs favs)
    (my-dbx-favorites--render)
    (if (null favs)
        (message "dbx: no favorites in %s yet; press * on a job page or a here"
                 (abbreviate-file-name default-directory))
      (dolist (fav favs)
        (let ((key (cons (plist-get fav :id) (plist-get fav :context))))
          (dbx-job-latest-runs
           (car key) my-desktop-dbx-favorites-runs
           (lambda (runs) (funcall done key (list :runs runs)))
           :context (cdr key)
           :error-callback (lambda (m) (funcall done key (list :error m)))))))))

;;;; Star from dbx pages

(dolist (mode '(dbx-job-page-mode dbx-run-page-mode
                dbx-jobs-list-mode dbx-runs-list-mode))
  (setf (alist-get mode dbx-page-extra-actions)
        '(("*" "Star / unstar the job" my-dbx-favorite-toggle))))

;; The dbx menu (C-c d d) gets the dashboard ("R" is a stable anchor).
(with-eval-after-load 'dbx
  (ignore-errors
    (transient-append-suffix 'dbx-dispatch "R"
      '("f" "favorites" my-dbx-favorites))))

(provide 'desktop-dbx-favorites)
;;; desktop-dbx-favorites.el ends here
