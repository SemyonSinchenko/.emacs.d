;;; desktop-rss.el --- RSS: elfeed + elfeed-org for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from the old IDE tools-rss.el (reader parts only).

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed"
                                              user-emacs-directory))
  (setq elfeed-search-filter "@2-weeks-ago +unread"))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files
        (mapcar #'my-desktop--expand
                (append my-desktop-elfeed-org-files nil))))

(when my-desktop-enable-elfeed-tube
  (use-package elfeed-tube
    :ensure t
    :after elfeed
    :config
    (elfeed-tube-setup)))

(defun my-rss-open ()
  "Open elfeed."
  (interactive)
  (elfeed))

(provide 'desktop-rss)
;;; desktop-rss.el ends here
