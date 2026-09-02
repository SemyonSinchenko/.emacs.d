;;; desktop-org.el --- Org / org-roam for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Migrated from the IDE `tools-org.el'.  The Desktop daemon is the
;; single owner of the org-roam database.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(use-package org
  :ensure t
  :custom
  (org-directory (my-desktop--expand my-desktop-org-dir))
  (org-agenda-files (mapcar #'my-desktop--expand
                            (append my-desktop-org-agenda-files nil)))
  :hook
  (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t)
  (setq org-startup-folded 'content))

(when my-desktop-enable-org-modern
  (use-package org-modern
    :ensure t
    :after org
    :hook (org-mode . org-modern-mode)
    :custom (org-modern-star 'replace)))

(when my-desktop-enable-org-roam
  (use-package org-roam
    :ensure t
    :custom
    (org-roam-directory (file-truename
                         (my-desktop--expand my-desktop-org-roam-dir)))
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert))
    :config
    ;; Fresh machines may not have the directory yet.
    (condition-case nil
        (make-directory org-roam-directory t)
      (error nil))
    (org-roam-db-autosync-mode)))

(provide 'desktop-org)
;;; desktop-org.el ends here
