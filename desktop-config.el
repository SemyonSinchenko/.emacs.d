;;; desktop-config.el --- Desktop settings: the one file you edit -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This is the SINGLE configuration file of the Emacs Desktop --
;; the equivalent of config.json / settings.yaml in other tools.
;; Everything here is plain `setq'.  No logic, no requires.
;;
;; It is loaded BEFORE any machinery module runs, so your values
;; always win over the defaults defined in
;; modules/desktop-config-defs.el (the full schema with docstrings).
;;
;; After editing: M-x my-desktop-reload-config
;; (keybindings/menu/banner apply live; frame and package settings
;; need a daemon restart).
;;
;; Machine-specific paths/secrets belong to desktop-config-local.el
;; (same format, git-ignored, optional).

;;; Code:

;; ------------------------------------------------------------------
;; 1. Paths
;; ------------------------------------------------------------------

(setq my-desktop-org-dir "~/Org")
(setq my-desktop-org-roam-dir "~/Org/org-roam/org-files/")
;; (setq my-desktop-org-agenda-files '("~/Org/agenda.org"))
(setq my-desktop-manga-dir "~/Nextcloud/Books/Manga/")
(setq my-desktop-music-dir "~/Nextcloud/Music/")
(setq my-desktop-downloads-dir "~/Downloads")
(setq my-desktop-reading-log-file "~/Org/reading.org")
;; Reader positions survive restarts via save-place; closing a
;; document appends a READ entry to the log file above:
(setq my-desktop-reading-log-auto t)
;; emacs-reader: build once, then point the desktop at it:
;;   git clone https://codeberg.org/MonadicSheep/emacs-reader ~/src/emacs-reader
;;   cd ~/src/emacs-reader && make clean all
(setq my-desktop-reader-dir "~/github/bins/emacs-reader")

;; ------------------------------------------------------------------
;; 2. Appearance
;; ------------------------------------------------------------------

(setq my-desktop-theme-flavor 'frappe)
(setq my-desktop-font "Ubuntu Mono-16")   ; "Family-SIZE"; raise SIZE for bigger
(setq my-desktop-frame-opacity 85)      ; <100: wallpaper shows through (lower = clearer)
(setq my-desktop-frame-undecorated t)
(setq my-desktop-frame-maximized t)
(setq my-desktop-show-menubar nil)
;; (setq my-desktop-use-icons t)         ; needs a Nerd Font installed
(setq my-desktop-display-time t)
(setq my-desktop-display-battery t)
;; (setq my-desktop-banner-image "~/Pictures/desktop-banner.png")
(setq my-desktop-banner-title "Desktop")

;; ------------------------------------------------------------------
;; 3. Workspaces (perspective.el) and sidebar (treemacs)
;; ------------------------------------------------------------------

(setq my-desktop-workspaces '("WS 1"))
(setq my-desktop-default-workspace "WS 1")
;; (setq my-desktop-treemacs-root "~/")  ; sidebar root in each workspace
(setq my-desktop-sidebar-autoshow nil) ; show with C-c ` when needed
(setq my-desktop-sidebar-fraction 6)   ; sidebar = 1/6 of frame width

;; ------------------------------------------------------------------
;; 4. Terminal
;; ------------------------------------------------------------------

(setq my-desktop-enable-ghostel t)
;; (setq my-desktop-term-default-dir "~/github")

;; ------------------------------------------------------------------
;; 5. Module flags
;; ------------------------------------------------------------------

(setq my-desktop-enable-media t)
;; Music lives in listen.el (C-c p = pause/resume, C-c m = library,
;; M-x listen = full player menu).  Directory: my-desktop-music-dir.
(setq my-desktop-enable-reader t)
(setq my-desktop-enable-rss t)
(setq my-desktop-enable-elfeed-tube t)
(setq my-desktop-enable-dirvish t)
(setq my-desktop-enable-popper t)
(setq my-desktop-enable-org-roam t)
(setq my-desktop-enable-org-modern t)
(setq my-desktop-enable-ai t)
(setq my-desktop-enable-torrent nil)    ; needs transmission-daemon
(setq my-desktop-enable-telega t)
;; Server options (pick one):
;;   native: install TDLib, then M-x telega-server-build (once)
;;   docker: (setq my-desktop-telega-docker t)   ; needs docker CLI
;; Optional MTProto/SOCKS5 proxy if Telegram needs one:
;; (setq my-desktop-telega-proxies
;;       '((( :server "1.2.3.4" :port 8080 :enable t
;;            :type (:@type "proxyTypeSocks5" :username "u" :password "p")))))
(setq my-desktop-enable-quelpa nil)
(setq my-desktop-enable-apheleia t)     ; format on save in configs, C-x x f
(setq my-desktop-enable-lexicon t)      ; AI text transforms, needs my-desktop-lexicon-dir

;; ------------------------------------------------------------------
;; 5b. Social feeds
;; ------------------------------------------------------------------

(setq my-desktop-enable-lem t)
;; Your Lemmy instance (find one at https://join-lemmy.org):
(setq my-desktop-lem-instance-url "https://lemmy.ml")
(setq my-desktop-lem-username "sem")
(setq my-desktop-enable-bluesky t)      ; needs ~/.authinfo entry
(setq my-desktop-enable-hackernews t)
(setq my-desktop-enable-reddit t)
;; Reddit needs the browser-gt extension in Firefox/Chrome and a
;; logged-in reddit.com tab; subreddits for the main view:
;; (setq my-desktop-reddit-subs '("emacs" "lisp" "linux"))

;; ------------------------------------------------------------------
;; 6. RSS
;; ------------------------------------------------------------------

(setq my-desktop-elfeed-org-files '("~/Org/feeds.org"))

;; ------------------------------------------------------------------
;; 7. Sync (rclone jobs for the C-c m menu)
;; ------------------------------------------------------------------

(setq my-desktop-sync-jobs
      '((:name "semsync"
               :remote "semdav:/"
               :local "~/Org/"
               :args ("--create-empty-src-dirs" "--fast-list" "-v"
                      "--exclude" "elfeed/data/**"))))

;; ------------------------------------------------------------------
;; 8. Torrent (transmission-daemon RPC)
;; ------------------------------------------------------------------

(setq my-desktop-torrent-host "localhost")
(setq my-desktop-torrent-port 9091)

;; ------------------------------------------------------------------
;; 9. AI assistant (gptel).  API keys come from environment
;;    variables named in :key-env -- never put keys in this file.
;; ------------------------------------------------------------------

(setq my-desktop-ai-backends
      '(;; DeepSeek (native gptel backend, key from the environment):
        (:name "deepseek"
          :type deepseek
          :key-env "DEEPSEEK_API_KEY"
          :models (deepseek-v4-flash deepseek-v4-pro)  ; first = default
          :stream t)
        ))
(setq my-desktop-ai-directives
      '(("default" . "You are a concise, helpful assistant.")))
(setq my-desktop-ai-search 'searxng)
(setq my-desktop-ai-searxng-url "http://localhost:7331/search")

;; Z-AI coding plan usage dashboard (C-c a z); key comes from the
;; environment variable named below:
(setq my-desktop-ai-zai-api-url "https://api.z.ai")
(setq my-desktop-ai-zai-api-key-env "ZAI_API_KEY")

;; ------------------------------------------------------------------
;; 9b. Roguelike copilot
;; ------------------------------------------------------------------

(setq my-desktop-roguelike-games
      '(("NetHack" . (:command "nethack"))
        ;; ("Angband" . (:command "angband"))
        ))
;; (setq my-desktop-roguelike-default-game "NetHack")
;; (setq my-desktop-roguelike-model 'deepseek-v4-flash)

;; ------------------------------------------------------------------
;; 10. Home tab
;; ------------------------------------------------------------------

;; (setq my-desktop-home-actions
;;       '(("Terminal" . my-term-new)
;;         ("Manga" . my-reader-open-manga)))
;; (setq my-desktop-home-folders
;;       '(("Org" . "~/Org")
;;         ("Downloads" . "~/Downloads")))

;; ------------------------------------------------------------------
;; 11. Extra keybindings (merged over machinery defaults)
;; ------------------------------------------------------------------

;; (setq my-desktop-keybindings
;;       '((global ("C-c z" . my-sidebar-toggle))
;;         (dired  ("C-c e" . my-reader-open-manga))))

;; ------------------------------------------------------------------
;; 12. Config/data editing (desktop-edit) and lexicon
;; ------------------------------------------------------------------

;; JSON/JS/TS indent width (apheleia's prettier follows it too):
;; (setq my-desktop-json-indent-level 2)
;; Markdown preview converter:
;; (setq my-desktop-markdown-command "multimarkdown")
;; CSV: alignment width and size limits for auto-align/rainbow colors:
;; (setq my-desktop-csv-align-width 40)
;; (setq my-desktop-csv-align-size-limit (* 10 1024 1024))
;; (setq my-desktop-rainbow-csv-size-limit (* 10 1024 1024))
;; lexicon-org location (AI text transforms, C-c l ...); the module
;; warns and stays off until the checkout exists:
;; (setq my-desktop-lexicon-dir "~/github/Lexicon/emacs")
;; lexicon-cli backend (LEXICON_INFERENCE); nil keeps the tool default:
;; (setq my-desktop-lexicon-inference "bundled")

(provide 'desktop-config)
;;; desktop-config.el ends here
