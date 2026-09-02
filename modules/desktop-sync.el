;;; desktop-sync.el --- rclone sync jobs and the C-c m menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient menu for the configured rclone jobs (semsync & co).
;; Jobs run asynchronously into *sync:<name>* buffers.

;;; Code:

(require 'transient)
(require 'desktop-config-defs)
(require 'desktop-core)

(defun my-sync--jobs ()
  "Configured sync jobs as an alist of (NAME . JOB-PLIST)."
  (mapcar (lambda (job)
            (cons (plist-get job :name) job))
          (append my-desktop-sync-jobs nil)))

(defun my-sync--buffer (name)
  (get-buffer-create (format "*sync:%s*" name)))

(defun my-sync--run (job &optional dry-run)
  "Run rclone for JOB asynchronously.  DRY-RUN adds -n."
  (unless (my-desktop--require-bin "rclone" "sync jobs")
    (user-error "rclone not found"))
  (let* ((name (plist-get job :name))
         (buffer (my-sync--buffer name))
         (args (append (list "sync"
                             (plist-get job :remote)
                             (my-desktop--expand (plist-get job :local)))
                       (append (plist-get job :args) nil)
                       (when dry-run '("-n")))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "$ rclone %s\n\n" (string-join args " "))))
    (message "[desktop] rclone %s %s..." (if dry-run "(dry-run)" name)
             (car (last args 2)))
    (set-process-sentinel
     (apply #'start-process (format "rclone-%s" name) buffer
            "rclone" args)
     (lambda (proc _event)
       (message "[desktop] rclone %s: %s"
                (process-name proc)
                (symbol-name (process-status proc)))))))

(defun my-sync-run (name)
  "Run sync job NAME (see `my-desktop-sync-jobs')."
  (interactive
   (list (completing-read "Sync job: " (my-sync--jobs) nil t)))
  (my-sync--run (cdr (assoc name (my-sync--jobs)))))

(defun my-sync-dry-run (name)
  "Dry-run sync job NAME."
  (interactive
   (list (completing-read "Dry-run job: " (my-sync--jobs) nil t)))
  (my-sync--run (cdr (assoc name (my-sync--jobs))) 'dry-run))

(defun my-sync-open-log (name)
  "Open the log buffer of job NAME."
  (interactive
   (list (completing-read "Log of job: " (my-sync--jobs) nil t)))
  (pop-to-buffer (my-sync--buffer name)))

(defun my-sync-kill ()
  "Kill running rclone processes started by this menu."
  (interactive)
  (let ((n 0))
    (dolist (proc (process-list))
      (when (string-prefix-p "rclone-" (process-name proc))
        (delete-process proc)
        (cl-incf n)))
    (message "[desktop] killed %d rclone process(es)" n)))

(transient-define-prefix my-sync-menu ()
  "Rclone sync jobs."
  [["Jobs"
    ("r" "Run job" my-sync-run)
    ("d" "Dry-run job" my-sync-dry-run)]
   ["Logs"
    ("l" "Open job log" my-sync-open-log)
    ("k" "Kill running jobs" my-sync-kill)]])

(provide 'desktop-sync)
;;; desktop-sync.el ends here
