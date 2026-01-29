(provide 'tools-org)

;; Org-roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Nextcloud/ORG/orgnote/"))
  :config
  (org-roam-db-autosync-mode))

(use-package websocket :after org-roam)
(use-package emacsql :after org-roam)

(use-package org-roam-ui
  :after emacsql
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Reader (EPUB)
(use-package reader
  :vc (:url "https://codeberg.org/divyaranjan/emacs-reader" :make "all"))
