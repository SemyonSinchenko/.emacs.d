;;; desktop-config-defs.el --- Desktop settings schema and defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Central schema of every user-facing setting of the Emacs Desktop.
;; Machinery (desktop-init.el + modules) NEVER hard-codes tunables:
;; it reads them from here.  `desktop-config.el' (the user file) is
;; loaded BEFORE modules, and `defcustom' never overwrites an
;; already-bound value -- so user settings always win by construction.
;;
;; This file defines defaults only.  Do not put logic here.

;;; Code:

(defgroup my-desktop nil
  "Emacs Desktop: a GUI desktop-shell built on Emacs."
  :group 'environment)

;; ------------------------------------------------------------------
;; 1. Paths
;; ------------------------------------------------------------------

(defcustom my-desktop-org-dir "~/Org"
  "Root directory of Org files (synced via rclone)."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-org-roam-dir "~/Org/org-roam/org-files/"
  "Directory of the org-roam knowledge base."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-org-agenda-files nil
  "List of org files with TODOs for the agenda widget.
Nil disables the agenda widget on the Home tab."
  :type '(repeat file)
  :group 'my-desktop)

(defcustom my-desktop-manga-dir nil
  "Directory with manga/comics (CBZ/CBR).  Nil = prompt each time."
  :type '(choice directory (const nil))
  :group 'my-desktop)

(defcustom my-desktop-music-dir nil
  "Directory with the music library.  Nil = prompt each time."
  :type '(choice directory (const nil))
  :group 'my-desktop)

(defcustom my-desktop-downloads-dir "~/Downloads"
  "Default downloads directory."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-ai-attachments-dir
  (expand-file-name "ai-attachments" user-emacs-directory)
  "Directory the AI assistant is allowed to read files from.
Copy (or save) files here before asking the assistant about them."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-ai-memory-file
  (expand-file-name "ai-memory.org" user-emacs-directory)
  "Org file used as the AI assistant long-term memory."
  :type 'file
  :group 'my-desktop)

(defcustom my-desktop-reading-log-file nil
  "Org file receiving reading-log entries.  Nil disables logging."
  :type '(choice file (const nil))
  :group 'my-desktop)

(defcustom my-desktop-reading-log-auto t
  "Append an entry to the reading log automatically when a
document opened in emacs-reader is closed."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-reader-dir nil
  "Directory of a locally built emacs-reader (the folder that
contains reader.el), e.g. \"~/src/emacs-reader\".  When set, it is
added to `load-path' so the reader module can require it.  Build:
clone https://codeberg.org/MonadicSheep/emacs-reader and run
\"make all\" (needs gcc, make, and mupdf >= 1.26 headers)."
  :type '(choice directory (const nil))
  :group 'my-desktop)

(defcustom my-desktop-reader-mode-alist nil
  "Extra `auto-mode-alist' entries for document formats.
Each entry is (REGEX . MODE-SYMBOL); for example a regexp
matching CBZ files mapped to the emacs-reader mode.  Entries
are only applied when the mode exists.  emacs-reader adds its
own associations once it is installed.
Only applied when the mode exists; emacs-reader provides its own
associations once installed."
  :type '(repeat cons)
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 2. Appearance
;; ------------------------------------------------------------------

(defcustom my-desktop-font nil
  "Default font, e.g. \"JetBrainsMono Nerd Font-11\".  Nil = don't set."
  :type '(choice string (const nil))
  :group 'my-desktop)

(defcustom my-desktop-frame-opacity 96
  "Frame text-background opacity in percent (1-100).
Below 100 the GNOME wallpaper shows through.  100 = opaque."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-frame-undecorated t
  "Create frames without WM decorations (borderless)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-frame-maximized t
  "Maximize frames on creation."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-show-menubar nil
  "Show the menu bar.  Nil hides menu/tool/scroll bars."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-theme-flavor 'frappe
  "Catppuccin flavor: latte, frappe, macchiato, mocha, or none."
  :type '(choice (const latte) (const frappe) (const macchiato)
                 (const mocha) (const none))
  :group 'my-desktop)

(defcustom my-desktop-use-icons nil
  "Use nerd-icons (requires a Nerd Font installed system-wide)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-display-time t
  "Show clock in the mode line."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-display-battery t
  "Show battery status in the mode line (when a battery exists)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-banner-image nil
  "Path to an image (png/jpg/gif) shown on the Home tab.
Nil uses the built-in text banner."
  :type '(choice file (const nil))
  :group 'my-desktop)

(defcustom my-desktop-banner-title "Desktop"
  "Title string shown on the Home tab."
  :type 'string
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 3. Workspaces and session
;; ------------------------------------------------------------------

(defcustom my-desktop-workspaces '("Workspace 1")
  "Ordered list of workspace (perspective) names.
Every session starts fresh with this list; rely on add / delete /
rename (sidebar keys and C-c w ...) to shape it."
  :type '(repeat string)
  :group 'my-desktop)

(defcustom my-desktop-default-workspace "Workspace 1"
  "Name of the workspace the session starts in."
  :type 'string
  :group 'my-desktop)

(defcustom my-desktop-treemacs-root "~"
  "Root directory the treemacs sidebar offers in every workspace.
It is seeded once per workspace, so the sidebar never asks for a
root project on first show."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-sidebar-autoshow nil
  "Open the treemacs sidebar automatically on the first frame.
When nil (the default), press \"C-c `\" once to show it; the root
project is seeded either way, so treemacs never asks for one."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-sidebar-width nil
  "Fixed sidebar width in columns.  Nil = derive from fraction."
  :type '(choice integer (const nil))
  :group 'my-desktop)

(defcustom my-desktop-sidebar-fraction 6
  "Sidebar takes 1/N of the frame width (used when width is nil)."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-first-frame-hook nil
  "Functions run once on the first frame, after workspaces,
sidebar and Home are set up.  Set in desktop-config-local.el to
open per-machine startup content."
  :type 'hook
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 4. Terminal
;; ------------------------------------------------------------------

(defcustom my-desktop-term-default-dir nil
  "Default directory for new terminals.  Nil = current default."
  :type '(choice directory (const nil))
  :group 'my-desktop)

(defcustom my-desktop-enable-ghostel t
  "Use ghostel (libghostty) terminals.  See M-x ghostel-download-module."
  :type 'boolean
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 5. Module flags (off = module not loaded at all)
;; ------------------------------------------------------------------

(defcustom my-desktop-enable-media t
  "Load the media module (listen.el + mpv)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-reader t
  "Load the reading module (nov.el; emacs-reader when installed)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-rss t
  "Load the RSS module (elfeed + elfeed-org)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-lem t
  "Load the Lemmy client (lem.el).  Set `my-desktop-lem-instance-url'."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-lem-instance-url nil
  "URL of your Lemmy instance, e.g. \"https://lemmy.ml\".
Nil: `lem' asks or errors on first use -- set it in
desktop-config.el."
  :type '(choice string (const nil))
  :group 'my-desktop)

(defcustom my-desktop-lem-username nil
  "Your Lemmy username (optional; skips the username prompt)."
  :type '(choice string (const nil))
  :group 'my-desktop)

(defcustom my-desktop-enable-bluesky t
  "Load the Bluesky client.
Credentials via auth-source: machine bsky.social, login = your
handle, password = an app password."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-hackernews t
  "Load the Hacker News client."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-elfeed-tube t
  "Load elfeed-tube (YouTube integration for elfeed)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-dirvish t
  "Enhance Dired with dirvish (previews, richer UI)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-popper t
  "Load popper (toggleable popup windows)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-org-roam t
  "Enable org-roam (Desktop daemon becomes the single DB owner)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-org-modern t
  "Enable org-modern visuals in Org buffers."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-reader-install nil
  "Reserved: install emacs-reader automatically (not implemented yet).
Until then: build it yourself, see
https://codeberg.org/MonadicSheep/emacs-reader (needs mupdf >= 1.26)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-torrent nil
  "Load the torrent module (transmission.el + transmission-daemon)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-telega nil
  "Load telega (Telegram client).  Needs a telega-server: either
built locally against TDLib, or run in Docker -- see
`my-desktop-telega-docker'."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-telega-emoji-images nil
  "When non-nil, telega renders emoji as SVG images via librsvg.
Keep nil (default): emoji are shown as plain Unicode text, which
renders correctly through Emacs' font stack.  Set t only if your
librsvg renders telega's emoji SVGs correctly on your machine."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-telega-docker nil
  "When non-nil, telega runs its server in the
zevlg/telega-server Docker container instead of a locally built
one.  Requires the `docker' CLI (podman users: podman-docker)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-telega-proxies nil
  "Optional MTProto proxies for telega, e.g. when Telegram is
blocked.  Each entry is a plist as documented in
`telega-proxies'.  Example:
  (((:server \"1.2.3.4\" :port 8080 :enable t
     :type (:@type \"proxyTypeSocks5\" :username \"u\"
            :password \"p\"))))"
  :type '(repeat plist)
  :group 'my-desktop)

(defcustom my-desktop-enable-ai t
  "Load the gptel-based assistant module."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-quelpa nil
  "Bootstrap quelpa at startup (needed only for git-only packages)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-apheleia t
  "Format code/config buffers with apheleia: automatically in
JSON/YAML/TOML buffers, on demand with C-x x f.  Formatter wiring
mirrors modules/lang-lsp.el (ruff for Python)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-enable-lexicon t
  "Load the lexicon-org module (AI text transforms, C-c l ...).
Needs the local checkout from `my-desktop-lexicon-dir'."
  :type 'boolean
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 6. RSS
;; ------------------------------------------------------------------

(defcustom my-desktop-elfeed-org-files '("~/Org/feeds.org")
  "Org files defining elfeed subscriptions (elfeed-org)."
  :type '(repeat file)
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 7. Sync (rclone)
;; ------------------------------------------------------------------

(defcustom my-desktop-sync-jobs
  '((:name "semsync"
           :remote "semdav:/"
           :local "~/Org/"
           :args ("--create-empty-src-dirs" "--fast-list" "-v"
                  "--exclude" "elfeed/data/**")))
  "List of rclone jobs shown in the sync menu.
Each job is a plist:
  :name   label shown in menus
  :remote rclone remote path (source)
  :local  local directory (destination, ~ is expanded)
  :args   list of extra rclone arguments."
  :type '(repeat plist)
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 8. Torrent
;; ------------------------------------------------------------------

(defcustom my-desktop-torrent-host "localhost"
  "Host running the transmission-daemon RPC."
  :type 'string
  :group 'my-desktop)

(defcustom my-desktop-torrent-port 9091
  "Port of the transmission-daemon RPC."
  :type 'integer
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 9. AI assistant (gptel)
;; ------------------------------------------------------------------

(defcustom my-desktop-ai-backends nil
  "List of AI backends.  The first with a key present wins.
API keys ALWAYS come from environment variables (:key-env) --
never write them into this file.

Each entry is a plist:
  :name     label (e.g. \"openrouter\")
  :type     \'openai (default: any OpenAI-compatible API) or
            \'deepseek (gptel\\='s native backend; host, endpoint and
            model defaults are built in, :host/:endpoint ignored)
  :host     host, e.g. \"openrouter.ai\" (no scheme; :type openai)
  :endpoint optional path, e.g. \"/api/v1/chat/completions\"
  :key-env  name of the environment variable holding the API key
  :models   list of model symbols, first is default (optional for
            :type deepseek)
  :stream   t for streaming (optional)

Examples:
  ((:name \"deepseek\"
           :type deepseek
           :key-env \"DEEPSEEK_API_KEY\"
           :stream t)
   (:name \"openrouter\"
           :host \"openrouter.ai\"
           :endpoint \"/api/v1/chat/completions\"
           :key-env \"OPENROUTER_API_KEY\"
           :models (anthropic/claude-3.5-sonnet)
           :stream t))"
  :type '(repeat plist)
  :group 'my-desktop)

(defcustom my-desktop-ai-directives
  '(("default" . "You are a concise, helpful assistant."))
  "Alist of personas: NAME . SYSTEM-PROMPT."
  :type '(alist :key-type string :value-type string)
  :group 'my-desktop)

(defcustom my-desktop-enable-reddit t
  "Load the Reddit client (reddigg).  Requires the browser-gt
Emacs package (auto-installed), the browser-gt browser extension,
and a logged-in Reddit session in that browser."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-reddit-subs nil
  "Subreddits shown by reddigg's main view,
e.g. \='(\"emacs\" \"lisp\" \"linux\")."
  :type '(repeat string)
  :group 'my-desktop)

(defcustom my-desktop-ai-sessions-dir
  (expand-file-name "ai-sessions" user-emacs-directory)
  "Directory where gptel chat sessions are persisted as Org
files.  Sessions are saved automatically after every response."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-ai-fetch-limit 10000
  "Characters per web_fetch call.  Long pages are paged: the
tool result states the offset to continue from."
  :type 'natnum
  :group 'my-desktop)

(defcustom my-desktop-ai-search 'none
  "Web-search backend for the assistant: none or searxng."
  :type '(choice (const none) (const searxng))
  :group 'my-desktop)

(defcustom my-desktop-ai-searxng-url nil
  "SearXNG instance URL providing JSON output, e.g.
\"http://localhost:8888/search\".  Required when search is searxng."
  :type '(choice string (const nil))
  :group 'my-desktop)

(defcustom my-desktop-ai-search-limit 5
  "Maximum search results returned to the assistant."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-ai-attachment-max-size (* 2 1024 1024)
  "Maximum file size the assistant may read via its tools."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-ai-zai-api-url "https://api.z.ai"
  "Base URL of the Z-AI monitor API used by the usage dashboard
(`my-ai-zai-usage', C-c a z)."
  :type 'string
  :group 'my-desktop)

(defcustom my-desktop-ai-zai-api-key-env "ZAI_API_KEY"
  "Environment variable that holds the Z-AI coding-plan API key."
  :type 'string
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; Roguelike copilot (NetHack etc.)
;; ------------------------------------------------------------------

(defcustom my-desktop-enable-roguelike t
  "Load the roguelike copilot (ghostel game + gptel advisor)."
  :type 'boolean
  :group 'my-desktop)

(defcustom my-desktop-roguelike-games
  '(("NetHack" . (:command "nethack")))
  "Games the roguelike copilot can launch.
Each entry is (NAME . PLIST):
  :command shell command typed into the terminal to start the game
  :dir      optional working directory for the terminal
  :profile  optional copilot profile key (defaults to lowercased
            NAME; profiles are defined in desktop-roguelike.el)

Examples:
  ((\"NetHack\" . (:command \"nethack\"))
   (\"Angband\" . (:command \"angband\" :dir \"~/angband\")))"
  :type '(alist :key-type string :value-type plist)
  :group 'my-desktop)

(defcustom my-desktop-roguelike-default-game nil
  "Default game name for `my-roguelike-start' (nil = prompt)."
  :type '(choice string (const nil))
  :group 'my-desktop)

(defcustom my-desktop-roguelike-log-dir
  (expand-file-name "roguelike" user-emacs-directory)
  "Directory holding roguelike copilot session data (message log,
compaction chunks, state card, stories).  Git-ignored state."
  :type 'directory
  :group 'my-desktop)

(defcustom my-desktop-roguelike-log-chunk-size 100
  "Compact the message log every this many new lines."
  :type 'natnum
  :group 'my-desktop)

(defcustom my-desktop-roguelike-recent-lines 100
  "Raw log lines injected as recent context into advisor questions."
  :type 'natnum
  :group 'my-desktop)

(defcustom my-desktop-roguelike-summary-count 4
  "How many recent compaction summaries to inject into context."
  :type 'natnum
  :group 'my-desktop)

(defcustom my-desktop-roguelike-model nil
  "gptel model (a symbol) used for advisor and compaction
requests.  Nil = the default model of the desktop AI backend."
  :type '(choice symbol (const nil))
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 10. Home tab widgets
;; ------------------------------------------------------------------

(defcustom my-desktop-home-actions
  '(("Terminal" . my-term-new)
    ("AI chat" . my-ai-chat)
    ("Music" . my-media-music-library)
    ("Manga" . my-reader-open-manga)
    ("Elfeed" . my-rss-open)
    ("Torrents" . my-torrent-open))
  "Action buttons on the Home tab: (LABEL . COMMAND).
Commands of disabled modules are skipped automatically."
  :type '(repeat (cons string symbol))
  :group 'my-desktop)

(defcustom my-desktop-home-folders nil
  "Folder shortcuts on the Home tab: (LABEL . DIRECTORY).
Each opens Dired.  Example:
  ((\"Org\" . \"~/Org\") (\"Downloads\" . \"~/Downloads\"))"
  :type '(repeat (cons string string))
  :group 'my-desktop)

(defcustom my-desktop-home-recents 8
  "Number of recent files on the Home tab."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-home-bookmarks 5
  "Number of bookmarks on the Home tab."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-home-projects 5
  "Number of projects on the Home tab."
  :type 'integer
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 11. Extra keybindings (merged over machinery defaults)
;; ------------------------------------------------------------------

(defcustom my-desktop-keybindings nil
  "Extra keybindings applied on top of machinery defaults.
Structure: list of (CONTEXT . ((KEY-STRING . COMMAND) ...)).
CONTEXT is `global', `dired', or `ghostel'.
COMMAND must be an existing command; missing commands are reported
as warnings but never break startup.

Example:
  ((global (\"C-c z\" . my-sidebar-toggle))
    (dired  (\"C-c e\" . my-reader-open-manga)))"
  :type '(repeat cons)
  :group 'my-desktop)

;; ------------------------------------------------------------------
;; 12. Config/data editing (desktop-edit) and lexicon
;; ------------------------------------------------------------------

(defcustom my-desktop-json-indent-level 2
  "Indent width for JSON/JS/TS buffers.
Sets `js-indent-level', `json-ts-mode-indent-offset' and
`typescript-ts-mode-indent-offset'; apheleia's prettier invocation
reads the same variables, as in the IDE."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-markdown-command "multimarkdown"
  "External command rendering Markdown previews (`markdown-command')."
  :type 'string
  :group 'my-desktop)

(defcustom my-desktop-csv-align-width 40
  "Alignment width passed to `csv-align-max-width' in CSV/TSV buffers."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-csv-align-size-limit (* 10 1024 1024)
  "Auto-align (csv-align-mode) only CSV/TSV buffers smaller than this."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-rainbow-csv-size-limit (* 10 1024 1024)
  "Enable rainbow-csv only in CSV/TSV buffers smaller than this."
  :type 'integer
  :group 'my-desktop)

(defcustom my-desktop-lexicon-dir "~/github/Lexicon/emacs"
  "Directory of the local lexicon-org checkout (the folder that
contains lexicon-org.el).  When it does not exist, the lexicon
module is skipped with a warning.  Set nil to disable explicitly."
  :type '(choice directory (const nil))
  :group 'my-desktop)

(defcustom my-desktop-lexicon-inference "bundled"
  "Value exported as the LEXICON_INFERENCE environment variable
for lexicon-cli.  \"bundled\" forces the local llama.cpp backend;
nil keeps the tool default."
  :type '(choice string (const nil))
  :group 'my-desktop)

(provide 'desktop-config-defs)
;;; desktop-config-defs.el ends here
