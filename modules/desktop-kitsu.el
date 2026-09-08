;;; desktop-kitsu.el --- Kitsu (kitsu.app) tracker UI in org -*- lexical-binding: t; -*-

;;; Commentary:
;; Kitsu (kitsu.app) manga/anime tracker inside the Emacs Desktop.
;;
;; - Dashboard: `my-kitsu-dashboard' opens an org file with one
;;   dynamic block per library section (current / planned /
;;   completed); C-c C-c on a block re-fetches and re-renders it.
;; - Bump: the [[kitsu-inc:ID][+]] / [[kitsu-dec:ID][-]] links PATCH
;;   the progress at the server and re-render the enclosing block.
;; - Search: saved queries as `kitsu-search' dynamic blocks
;;   (name, categories, min-rating client-side filter).
;; - Trending: weekly (kitsu.app "trending now") and all-time top-N
;;   as a `kitsu-trending' dynamic block.
;;
;; Auth: `my-desktop-kitsu-auth-file' (default ~/.emacs.d/.kitsuauth)
;; holds two lines, "user: ..." and "password: ...".  A missing or
;; malformed file fails fast with a clear message; there are no
;; prompts and no anonymous fallback.  Tokens are kept in memory
;; only (a fresh password grant on expiry; the refresh token is
;; ignored).  The file must stay git-ignored: the module refuses to
;; run when git tracks it and warns when it is not ignored.
;;
;; HTTP: synchronous curl subprocess (curl is a system dependency of
;; this config).  A custom User-Agent is REQUIRED: kitsu.io sits
;; behind Cloudflare, which answers 403 error 1010 to bot-like
;; signatures (the default url.el UA gets banned).

;;; Code:

(require 'desktop-config-defs)
(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'transient)
(require 'url)

;; Constants and state ------------------------------------------------

(defconst my-kitsu--client-id
  "dd031b32d2f56c990b1425efe6c42ad847e7fe3ab46bf1299f05ecd856434df6"
  "Public kitsu.app web client id, sent as the CLIENT-ID header.")

(defconst my-kitsu--user-agent "kitsu-emacs/1.0 (Emacs Desktop)"
  "User-Agent accepted by the Cloudflare proxy in front of kitsu.io.")

(defvar my-kitsu--token nil "Cached OAuth access token (memory only).")
(defvar my-kitsu--token-expiry 0.0 "Expiry of `my-kitsu--token', float time.")
(defvar my-kitsu--uid nil "Cached user id of the authenticated account.")
(defvar my-kitsu--auth-safety :unknown
  "Cached verdict of the git-safety check: :unknown, :ok or :tracked.")
(defvar my-kitsu--categories nil
  "Cached alist of (SLUG . TITLE) for category completion.")

;; Low-level HTTP -----------------------------------------------------

(defun my-kitsu--curl (method url headers &optional body)
  "Synchronous curl request.  Return (HTTP-STATUS . BODY-STRING).
BODY, when non-nil, is sent verbatim on stdin."
  (let ((args (list "--silent" "--compressed" "--max-time" "30"
                    "--write-out" "\n%{http_code}" "-X" method)))
    (dolist (h headers) (setq args (append args (list "-H" h))))
    (when body (setq args (append args (list "--data-binary" "@-"))))
    (setq args (append args (list url)))
    (with-temp-buffer
      (when body (insert body))
      ;; DELETE=t: the stdin region must be deleted before curl's
      ;; output is inserted, otherwise body and response concatenate.
      (let* ((exit (apply #'call-process-region
                          (point-min) (point-max) "curl" t t nil args))
             (out (buffer-string)))
        (when (/= exit 0)
          (error "Kitsu: curl failed (exit %s) for %s" exit url))
        (let ((m (string-match "\n[0-9][0-9][0-9]\\'" out)))
          (if m
              (cons (string-to-number (substring out (1+ m)))
                    (substring out 0 m))
            (cons 0 out)))))))

(defun my-kitsu--headers (&optional token content-type)
  "Build the request headers; TOKEN and CONTENT-TYPE optional."
  (let ((hs (list "Accept: application/vnd.api+json"
                  (concat "CLIENT-ID: " my-kitsu--client-id)
                  (concat "User-Agent: " my-kitsu--user-agent))))
    (when token (push (concat "Authorization: Bearer " token) hs))
    (when content-type (push (concat "Content-Type: " content-type) hs))
    hs))

(defun my-kitsu--json-decode (str)
  "Decode STR as JSON into alists."
  (condition-case err
      (json-parse-string str :object-type 'alist :array-type 'list)
    (error
     ;; Truncate: never echo a whole (possibly sensitive) body.
     (error "Kitsu: undecodable JSON from server: %s (body starts: %.60s...)"
            (error-message-string err) str))))

(defun my-kitsu--request (method url &optional body token)
  "Run METHOD request against URL.  Return (STATUS . JSON-OR-NIL)."
  (let* ((headers (my-kitsu--headers token
                                     (and body "application/vnd.api+json")))
         (resp (my-kitsu--curl method url headers body))
         (raw (cdr resp)))
    (cons (car resp)
          (if (and raw (> (length raw) 0)) (my-kitsu--json-decode raw) nil))))

(defun my-kitsu--edge-url (path &optional params)
  "Full URL for an /edge PATH with query PARAMS (an alist)."
  (concat my-desktop-kitsu-api-base "/edge" path
          (and params
               (concat "?" (url-build-query-string
                            (mapcar (lambda (kv) (list (car kv) (cdr kv)))
                                    params))))))

(defun my-kitsu--api-error (body)
  "Extract a human-readable message from an error BODY."
  (cond
   ((and (listp body) (alist-get 'errors body))
    (let ((e (car (alist-get 'errors body))))
      (or (and e (or (alist-get 'detail e) (alist-get 'title e)))
          "server error")))
   ((alist-get 'error_description body))
   ((alist-get 'error body))
   (t "unknown error")))

(defun my-kitsu--api (method path &optional params body)
  "Authenticated request to /edge PATH.  Return (STATUS . JSON).
A 401 or 5xx answer triggers exactly one re-login and retry:
kitsu answers an expired token with 500, not 401."
  (let* ((url (my-kitsu--edge-url path params))
         (resp (my-kitsu--request method url body (my-kitsu--ensure-token))))
    (when (or (>= (car resp) 500) (eq (car resp) 401))
      (setq my-kitsu--token nil my-kitsu--token-expiry 0)
      (setq resp (my-kitsu--request method url body (my-kitsu--ensure-token))))
    resp))

;; Authentication -----------------------------------------------------

(defun my-kitsu--check-auth-file-safety (file)
  "Refuse git-tracked auth files; warn when not git-ignored."
  (when (eq my-kitsu--auth-safety :tracked)
    (user-error "Kitsu: %s is tracked by git -- run:  git rm --cached %s"
                file file))
  (when (eq my-kitsu--auth-safety :unknown)
    (let* ((dir (directory-file-name (file-name-directory
                                      (expand-file-name file))))
           (name (file-name-nondirectory file))
           (git (executable-find "git"))
           (repo (and git
                      (eq 0 (call-process git nil nil nil
                                          "-C" dir "rev-parse" "--git-dir")))))
      (setq my-kitsu--auth-safety
            (cond
             ((not repo) :ok)
             ((eq 0 (call-process git nil nil nil
                                  "-C" dir "ls-files" "--error-unmatch" name))
              :tracked)
             ((eq 0 (call-process git nil nil nil
                                  "-C" dir "check-ignore" "-q" name))
              :ok)
             (t
              (message "Kitsu: %s is NOT git-ignored -- add it to .gitignore"
                       file)
              :ok)))))
  (when (eq my-kitsu--auth-safety :tracked)
    (user-error "Kitsu: %s is tracked by git -- run:  git rm --cached %s"
                file file)))

(defun my-kitsu--warn-perms (file)
  "Warn when FILE is readable by group/others."
  (when (/= 0 (logand (file-modes file) #o077))
    (message "Kitsu: %s is readable by others; consider: chmod 600 %s"
             file file)))

(defun my-kitsu--read-authfile ()
  "Return (USER . PASSWORD) from the auth file; fast-fail otherwise."
  (let ((file (expand-file-name my-desktop-kitsu-auth-file)))
    (unless (file-exists-p file)
      (user-error
       "Kitsu: auth file not found: %s (create it with two lines: \"user: ...\" and \"password: ...\")"
       file))
    (my-kitsu--warn-perms file)
    (my-kitsu--check-auth-file-safety file)
    (let (vals)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (setq line (string-trim line))
          (unless (or (= 0 (length line)) (eq (aref line 0) ?#))
            (let ((pos (string-search ":" line)))
              (unless pos
                (user-error "Kitsu: %s: line without \"key: value\": %S"
                            file line))
              (push (cons (downcase (string-trim (substring line 0 pos)))
                          (string-trim (substring line (1+ pos))))
                    vals)))))
      (dolist (key '("user" "password"))
        (unless (alist-get key vals nil nil #'equal)
          (user-error "Kitsu: %s is malformed: no \"%s: ...\" line" file key)))
      (cons (alist-get "user" vals nil nil #'equal)
            (alist-get "password" vals nil nil #'equal)))))

(defun my-kitsu--login ()
  "Password-grant login; store the token in memory and return it."
  (pcase-let ((`(,user . ,password) (my-kitsu--read-authfile)))
    (let* ((resp (my-kitsu--request
                  "POST" (concat my-desktop-kitsu-api-base "/oauth/token")
                  (json-encode `((grant_type . "password")
                                 (username . ,user)
                                 (password . ,password)))))
           (status (car resp))
           (body (cdr resp)))
      (unless (and (eq status 200) (alist-get 'access_token body))
        (user-error "Kitsu: login failed (HTTP %s): %s -- check %s"
                    status (my-kitsu--api-error body)
                    my-desktop-kitsu-auth-file))
      (setq my-kitsu--token (alist-get 'access_token body)
            ;; ~23 days server-side; refresh 2 min early.  The
            ;; refresh_token is deliberately ignored.
            my-kitsu--token-expiry
            (+ (float-time)
               (- (string-to-number
                   (format "%s" (or (alist-get 'expires_in body) 2000000)))
                  120)))
      my-kitsu--token)))

(defun my-kitsu--ensure-token ()
  "Return a valid token, logging in when needed."
  (when (or (null my-kitsu--token) (<= my-kitsu--token-expiry (float-time)))
    (my-kitsu--login))
  my-kitsu--token)

(defun my-kitsu--uid ()
  "User id of the authenticated account (cached)."
  (or my-kitsu--uid
      (setq my-kitsu--uid
            (let* ((resp (my-kitsu--api
                          "GET" "/users"
                          '(("filter[self]" . "true") ("page[limit]" . "1"))))
                   (data (and (eq 200 (car resp))
                              (alist-get 'data (cdr resp)))))
              (or (and data (alist-get 'id (car data)))
                  (error "Kitsu: cannot resolve user id (HTTP %s)"
                         (car resp)))))))

;; Fetch helpers ------------------------------------------------------

(defun my-kitsu--fetch-pages (path params &optional want)
  "GET PATH with PARAMS following page[offset] pagination.
WANT caps the number of entries (nil = fetch everything).  The
server caps pages at 20 for most endpoints; the loop adapts.
Return a cons (ENTRIES . INCLUDED)."
  (let ((data nil) (included nil) (offset 0) (page-size 500) done)
    (while (not done)
      (let* ((resp (my-kitsu--api
                    "GET" path
                    (append params
                            `(("page[limit]" . ,page-size)
                              ("page[offset]" . ,offset)))))
             (body (cdr resp)))
        (when (and (eq 400 (car resp)) (> page-size 20))
          ;; endpoints differ in page caps (library-entries allows 500,
          ;; most others 20): retry with the documented cap
          (setq page-size 20)
          (setq resp (my-kitsu--api
                      "GET" path
                      (append params
                              `(("page[limit]" . ,page-size)
                                ("page[offset]" . ,offset))))
              body (cdr resp)))
        (unless (eq 200 (car resp))
          (error "Kitsu: GET %s failed (HTTP %s)" path (car resp)))
        (let ((d (alist-get 'data body)))
          (setq data (append data d)
                included (append included (alist-get 'included body))
                done (or (null d) (= 0 (length d))
                         (and want (>= (length data) want))))
          (unless done
            (setq offset (+ offset (length d)))
            (when (< (length d) page-size) (setq page-size (length d)))))))
    (cons (if want (seq-take data want) data) included)))

(defun my-kitsu--included-map (included type)
  "Hash id -> record for INCLUDED records of TYPE."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (r included)
      (when (equal (alist-get 'type r) (symbol-name type))
        (puthash (alist-get 'id r) r map)))
    map))

(defun my-kitsu--entry-media-id (entry kind)
  "Media id related to library ENTRY of KIND."
  (alist-get 'id
             (alist-get 'data
                        (alist-get (intern kind)
                                   (alist-get 'relationships entry)))))

(defun my-kitsu--title (media)
  "Best-effort display title of MEDIA."
  (let ((a (alist-get 'attributes media)))
    (or (alist-get 'canonicalTitle a)
        (alist-get 'en (alist-get 'titles a))
        (alist-get 'slug a)
        "?")))

(defun my-kitsu--media-count (mattrs kind)
  "Total chapters/episodes of MATTRS for KIND, or nil when unknown."
  (alist-get (if (equal kind "anime") 'episodeCount 'chapterCount) mattrs))

(defun my-kitsu--human-count (n)
  "12345 -> \"12.3k\"."
  (cond ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
        ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
        (t (format "%s" n))))

(defun my-kitsu--date-only (iso)
  "\"2026-09-08T...\" -> \"2026-09-08\"."
  (if (and iso (> (length iso) 9)) (substring iso 0 10) (or iso "")))

(defun my-kitsu--clean (v)
  "Map JSON null (parsed as :null) to nil; return V otherwise."
  (if (eq v :null) nil v))

(defun my-kitsu--get (alist key &optional default)
  "ALIST-GET KEY with JSON null treated as missing (DEFAULT)."
  (or (my-kitsu--clean (alist-get key alist)) default))

(defun my-kitsu--as-number (v &optional default)
  "Coerce V (number or string) to a number, DEFAULT on nil."
  (cond ((numberp v) v)
        ((and v (not (eq v :null))) (string-to-number (format "%s" v)))
        (t (or default 0))))

(defun my-kitsu--param (params key default)
  "Stringify plist PARAMS value at KEY, with DEFAULT."
  (let ((v (plist-get params key)))
    (if v (format "%s" v) default)))

(defun my-kitsu--library-data (kind)
  "All library entries of KIND: cons (ENTRIES . MEDIA-MAP)."
  (pcase-let* ((`(,entries . ,included)
                (my-kitsu--fetch-pages
                 "/library-entries"
                 `(("filter[kind]" . ,kind)
                   ("filter[userId]" . ,(my-kitsu--uid))
                   ("include" . ,kind)))))
    (cons entries (my-kitsu--included-map included (intern kind)))))

;; Org rendering ------------------------------------------------------

(defun my-kitsu--section-title (status)
  "Human title for a Kitsu STATUS."
  (pcase status
    ("current" "In progress")
    ("planned" "Reading list")
    ("completed" "Completed")
    ("on_hold" "On hold")
    ("dropped" "Dropped")
    (_ status)))

(defun my-kitsu--insert-library-entry (entry kind media-map)
  "Insert one org entry (header + properties + action links)."
  (let* ((attrs (alist-get 'attributes entry))
         (id (alist-get 'id entry))
         (media-id (my-kitsu--entry-media-id entry kind))
         (media (and media-id (gethash media-id media-map)))
         (mattrs (and media (alist-get 'attributes media)))
         (title (if media (my-kitsu--title media) "(media missing)"))
         (progress (my-kitsu--as-number
                    (my-kitsu--get attrs 'progress) 0))
         (total (and mattrs
                     (my-kitsu--as-number
                      (my-kitsu--clean (my-kitsu--media-count mattrs kind))
                      nil)))
         (pct (and total (> total 0) (/ (* 100 progress) total)))
         (rating (my-kitsu--clean (alist-get 'ratingTwenty attrs))))
    (insert (format "** %s -- %s %d/%s%s\n"
                    title (if (equal kind "anime") "ep" "ch")
                    progress (if total (number-to-string total) "?")
                    (if pct (format " (%d%%)" pct) "")))
    (org-set-property "KITSU_ENTRY" id)
    (when media-id (org-set-property "KITSU_MEDIA" media-id))
    (org-set-property "KITSU_KIND" kind)
    (org-set-property "KITSU_PROGRESS" (number-to-string progress))
    (when total (org-set-property "KITSU_TOTAL" (number-to-string total)))
    (insert (format "  [[kitsu-inc:%s][+]] · [[kitsu-dec:%s][-]]" id id))
    (when media-id
      (insert (format " · [[kitsu-web:%s/%s][kitsu.app]]" kind media-id)))
    (when rating
      (insert (format " · my rating %s/10"
                      (/ (my-kitsu--as-number rating) 2.0))))
    (insert (format " · updated %s\n"
                    (my-kitsu--date-only
                     (my-kitsu--get attrs 'updatedAt ""))))))

(defun org-dblock-write:kitsu-library (params)
  "Render one dashboard section.
PARAMS: :kind manga|anime, :status one of current/planned/
completed/on_hold/dropped."
  (let* ((kind (downcase (my-kitsu--param params :kind "manga")))
         (status (my-kitsu--param params :status "current")))
    (pcase-let* ((`(,entries . ,media-map) (my-kitsu--library-data kind))
                 (sel (cl-sort
                       (cl-remove-if-not
                        (lambda (e)
                          (equal (my-kitsu--get
                                  (alist-get 'attributes e) 'status)
                                 status))
                        entries)
                       #'string>
                       :key (lambda (e)
                              (or (alist-get 'updatedAt
                                             (alist-get 'attributes e))
                                  "")))))
      (if (null sel)
          (insert "_none_\n")
        (dolist (e sel)
          (my-kitsu--insert-library-entry e kind media-map))))))

(defun my-kitsu--insert-search-hit (media rank kind &optional cat-map)
  "Insert one org entry for a search/trending MEDIA."
  (let* ((a (alist-get 'attributes media))
         (id (alist-get 'id media))
         (title (my-kitsu--title media))
         (rating (my-kitsu--as-number
                  (my-kitsu--get a 'averageRating) nil))
         (users (my-kitsu--clean (alist-get 'userCount a)))
         (subtype (my-kitsu--clean (alist-get 'subtype a)))
         (year (let ((d (my-kitsu--clean (alist-get 'startDate a))))
                 (and d (> (length d) 4) (substring d 0 4)))))
    (insert (format "** %d. %s\n" rank title))
    (org-set-property "KITSU_MEDIA" id)
    (org-set-property "KITSU_KIND" kind)
    (insert (format "  [[kitsu-add:%s][+ reading list]] · [[kitsu-start:%s][start now]] · [[kitsu-web:%s/%s][kitsu.app]]\n"
                    id id kind id))
    (insert (concat "  "
                    (when rating (format "★ %.1f  " (/ rating 10.0)))
                    (when users
                      (format "%s %s  " (my-kitsu--human-count users)
                              (if (equal kind "anime") "viewers" "readers")))
                    (when subtype (format "%s  " subtype))
                    (when year (format "%s" year))
                    "\n"))
    (when cat-map
      (let* ((rel (alist-get 'categories (alist-get 'relationships media)))
             (ids (mapcar (lambda (d) (alist-get 'id d))
                          (alist-get 'data rel)))
             (names (delq nil
                          (mapcar (lambda (i)
                                    (let ((c (gethash i cat-map)))
                                      (and c (alist-get 'title
                                                        (alist-get 'attributes c)))))
                                  ids))))
        (when names
          (insert (format "  categories: %s\n"
                          (mapconcat #'identity (seq-take names 4) ", "))))))))

(defun org-dblock-write:kitsu-search (params)
  "Render search results for the spec on the #+BEGIN line.
PARAMS: :q, :kind manga|anime, :sort (server side, e.g.
-userCount or -averageRating), :categories slug1,slug2,
:min-rating 0-100 (client side), :limit N.  Org parses block
params with `read': quote values with spaces (:q \"one piece\")
or encode spaces as + (:q one+piece)."
  (let* ((q (my-kitsu--param params :q nil))
         (kind (downcase (my-kitsu--param params :kind "manga")))
         (sort (my-kitsu--param params :sort "-userCount"))
         (limit (string-to-number (my-kitsu--param params :limit "20")))
         (minr (let ((v (plist-get params :min-rating)))
                 (and v (string-to-number (format "%s" v)))))
         (cats (let ((v (plist-get params :categories)))
                 (and v (split-string (format "%s" v) "," t "[ \t]+")))))
    (unless (and q (> (length q) 0))
      (user-error "kitsu-search block needs :q on the #+BEGIN line (encode spaces as +)"))
    (let* ((api-params `(("filter[text]" . ,(replace-regexp-in-string "\+" " " q))
                         ("sort" . ,sort)
                         ("include" . "categories")))
           (api-params (if cats
                           (append api-params
                                   `(("filter[categories]"
                                      . ,(mapconcat #'identity cats ","))))
                         api-params))
           (resp (my-kitsu--api "GET" (concat "/" kind) api-params)))
      (unless (eq 200 (car resp))
        (error "Kitsu: search failed (HTTP %s): %s"
               (car resp) (my-kitsu--api-error (cdr resp))))
      (let* ((body (cdr resp))
             (hits (alist-get 'data body))
             (total (alist-get 'count (alist-get 'meta body))))
        (when minr
          (setq hits
                (cl-remove-if-not
                 (lambda (m)
                   (>= (my-kitsu--as-number
                        (my-kitsu--get (alist-get 'attributes m)
                                       'averageRating)
                        0)
                       minr))
                 hits)))
        (setq hits (seq-take hits limit))
        (if (null hits)
            (insert "_no results_\n")
          (let ((cat-map (my-kitsu--included-map
                          (alist-get 'included body) 'categories))
                (i 0))
            (dolist (m hits)
              (setq i (1+ i))
              (my-kitsu--insert-search-hit m i kind cat-map))))
        (insert (format "_%d of %s result(s)%s_\n"
                        (length hits) (or total "?")
                        (if minr
                            (format ", client-side rating >= %s" minr)
                          "")))))))

(defun org-dblock-write:kitsu-trending (params)
  "Render a trending chart.  PARAMS: :kind manga|anime,
:period weekly|all, :limit N (for :all; default
`my-desktop-kitsu-trending-size')."
  (let* ((kind (downcase (my-kitsu--param params :kind "manga")))
         (period (downcase (my-kitsu--param params :period "weekly")))
         (limit (string-to-number
                 (my-kitsu--param params :limit
                                  (number-to-string
                                   my-desktop-kitsu-trending-size)))))
    (pcase period
      ("weekly"
       (let* ((resp (my-kitsu--api "GET" (format "/trending/%s" kind)
                                   '(("page[limit]" . "10"))))
              (body (cdr resp)))
         (unless (eq 200 (car resp))
           (error "Kitsu: trending failed (HTTP %s)" (car resp)))
         (insert "_kitsu.app trending now (about 10 items)_\n")
         (let ((i 0))
           (dolist (m (alist-get 'data body))
             (setq i (1+ i))
             (my-kitsu--insert-search-hit m i kind nil)))))
      ("all"
       (pcase-let* ((`(,hits . ,_)
                     (my-kitsu--fetch-pages (concat "/" kind)
                                            `(("sort" . "-userCount"))
                                            limit)))
         (insert (format "_all-time top %d by user count_\n"
                         (length hits)))
         (let ((i 0))
           (dolist (m hits)
             (setq i (1+ i))
             (my-kitsu--insert-search-hit m i kind nil)))))
      (_ (user-error "kitsu-trending :period must be weekly or all")))))

;; Actions ------------------------------------------------------------

(defun my-kitsu--set-progress-remote (entry-id new)
  "PATCH progress of ENTRY-ID to NEW at the server."
  (let* ((resp (my-kitsu--api
                "PATCH" (format "/library-entries/%s" entry-id) nil
                (json-encode `((data . ((type . "libraryEntries")
                                        (id . ,entry-id)
                                        (attributes . ((progress . ,new)))))))))
           (status (car resp)))
    (unless (eq status 200)
      (error "Kitsu: progress update failed (HTTP %s): %s"
             status (my-kitsu--api-error (cdr resp))))
    new))

(defun my-kitsu--refresh-dblock ()
  "Re-render the enclosing kitsu dynamic block.
`org-update-dblock' requires point exactly on the #+BEGIN line,
so find it (nearest BEGIN above point) and position there.  The
window point is kept as close as possible."
  (let* ((win (and (window-live-p (selected-window)) (selected-window)))
         (wpt (and win (window-point win))))
    (save-excursion
      (beginning-of-line)
      (unless (looking-at org-dblock-start-re)
        (unless (re-search-backward org-dblock-start-re nil t)
          (user-error "Kitsu: no dynamic block found above point"))
        (beginning-of-line))
      (condition-case err
          (org-update-dblock)
        (error (message "Kitsu: block refresh failed: %s"
                        (error-message-string err)))))
    (when win
      (set-window-point win (min (or wpt (point)) (point-max))))))

(defun my-kitsu--bump-at-point (entry-id delta)
  "Bump the entry at point by DELTA, then re-render its block."
  (org-back-to-heading t)
  (let* ((cur (string-to-number
               (or (org-entry-get nil "KITSU_PROGRESS" t) "0")))
         (new (my-kitsu--set-progress-remote entry-id (max 0 (+ cur delta)))))
    (org-entry-put nil "KITSU_PROGRESS" (number-to-string new))
    (my-kitsu--refresh-dblock)
    (message "Kitsu: progress -> %d" new)))

(defun my-kitsu-bump (delta)
  "Bump Kitsu progress of the entry at point by DELTA."
  (interactive "p")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org buffer"))
  (org-back-to-heading t)
  (let ((id (org-entry-get nil "KITSU_ENTRY" t)))
    (unless id (user-error "No KITSU_ENTRY property at point"))
    (my-kitsu--bump-at-point id delta)))

(defun my-kitsu-set-progress (new)
  "Set the progress of the Kitsu entry at point to NEW."
  (interactive "nNew progress: ")
  (org-back-to-heading t)
  (let ((id (org-entry-get nil "KITSU_ENTRY" t)))
    (unless id (user-error "No KITSU_ENTRY property at point"))
    (org-entry-put nil "KITSU_PROGRESS" (number-to-string new))
    (my-kitsu--set-progress-remote id new)
    (my-kitsu--refresh-dblock)
    (message "Kitsu: progress -> %d" new)))

(defun my-kitsu-set-status (status)
  "Set the library STATUS of the Kitsu entry at point and re-render."
  (interactive
   (list (completing-read "Status: "
                          '("current" "planned" "completed"
                            "on_hold" "dropped")
                          nil t "completed")))
  (org-back-to-heading t)
  (let ((id (org-entry-get nil "KITSU_ENTRY" t)))
    (unless id (user-error "No KITSU_ENTRY property at point"))
    (let* ((resp (my-kitsu--api
                  "PATCH" (format "/library-entries/%s" id) nil
                  (json-encode `((data . ((type . "libraryEntries")
                                          (id . ,id)
                                          (attributes . ((status . ,status)))))))))
           (code (car resp)))
      (unless (eq code 200)
        (error "Kitsu: status update failed (HTTP %s): %s"
               code (my-kitsu--api-error (cdr resp))))
      (my-kitsu--refresh-dblock)
      (message "Kitsu: status -> %s" status))))

(defun my-kitsu-open-web ()
  "Open the kitsu.app page of the media at point."
  (interactive)
  (org-back-to-heading t)
  (let ((kind (org-entry-get nil "KITSU_KIND" t))
        (mid (org-entry-get nil "KITSU_MEDIA" t)))
    (unless (and kind mid)
      (user-error "No Kitsu media at point"))
    (browse-url (format "https://kitsu.app/%s/%s" kind mid))))

(defun my-kitsu--library-add (kind media-id status)
  "Create a library entry for MEDIA-ID with STATUS."
  (let* ((resp (my-kitsu--api
                "POST" "/library-entries" nil
                (json-encode
                 `((data . ((type . "libraryEntries")
                            (attributes . ((status . ,status) (progress . 0)))
                            (relationships .
                                           ((,(intern kind) .
                                             ((data . ((type . ,(intern kind))
                                                       (id . ,media-id)))))))))))))
         (code (car resp)))
    (unless (memq code '(200 201))
      (error "Kitsu: adding to library failed (HTTP %s): %s%s"
             code (my-kitsu--api-error (cdr resp))
             (if (eq code 403)
                 " -- the server refuses entry creation for this
account (unverified or restricted account?)"
               "")))
    (alist-get 'id (alist-get 'data (cdr resp)))))

(defun my-kitsu--library-action-at-point (status)
  "Add the media at point to the library with STATUS."
  (org-back-to-heading t)
  (let ((kind (org-entry-get nil "KITSU_KIND" t))
        (mid (org-entry-get nil "KITSU_MEDIA" t)))
    (unless (and kind mid)
      (user-error "No Kitsu media at point"))
    (my-kitsu--library-add kind mid status)
    (message "Kitsu: added to library (%s)" status)))

;; Org links ----------------------------------------------------------

(org-link-set-parameters
 "kitsu-inc"
 :follow (lambda (path _p) (my-kitsu--bump-at-point path 1))
 :help-echo "Bump progress +1")

(org-link-set-parameters
 "kitsu-dec"
 :follow (lambda (path _p) (my-kitsu--bump-at-point path -1))
 :help-echo "Bump progress -1")

(org-link-set-parameters
 "kitsu-web"
 :follow (lambda (path _p) (browse-url (concat "https://kitsu.app/" path)))
 :help-echo "Open on kitsu.app")

(org-link-set-parameters
 "kitsu-add"
 :follow (lambda (_path _p) (my-kitsu--library-action-at-point "planned"))
 :help-echo "Add to reading list")

(org-link-set-parameters
 "kitsu-start"
 :follow (lambda (_path _p) (my-kitsu--library-action-at-point "current"))
 :help-echo "Start now")

;; Category completion (search blocks) ---------------------------------

(defun my-kitsu--categories ()
  "All category slugs (SLUG . TITLE), cached per session."
  (or my-kitsu--categories
      (setq my-kitsu--categories
            (pcase-let* ((`(,cats . ,_)
                          (my-kitsu--fetch-pages "/categories" nil)))
              (sort
               (delq nil
                     (mapcar (lambda (c)
                               (let ((a (alist-get 'attributes c)))
                                 (and (alist-get 'slug a)
                                      (cons (alist-get 'slug a)
                                            (alist-get 'title a)))))
                             cats))
               (lambda (x y) (string-lessp (car x) (car y))))))))

(defun my-kitsu--category-cap ()
  "Complete category slugs on :categories lines of search blocks."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (beginning-of-line)
               (looking-at "[ \t]*:categories\\b")))
    (let ((beg (save-excursion (skip-chars-backward "a-z0-9-") (point)))
          (end (point)))
      (list beg end (mapcar #'car (my-kitsu--categories))
            :exclusive 'no))))

;; Files and commands --------------------------------------------------

(defun my-kitsu--dashboard-skeleton ()
  "Initial dashboard file content, built from
`my-desktop-kitsu-dashboard-sections'."
  (concat
   "#+TITLE: Kitsu -- my library\n"
   "# C-c C-c on a block re-fetches it; M-x my-kitsu-dashboard refreshes all.\n"
   "# The + / - links bump progress at kitsu.app and re-render the block.\n"
   "# C-c C-k opens the entry menu at point.\n\n"
   (mapconcat
    (lambda (sec)
      (format "* %s\n#+BEGIN: kitsu-library :kind %s :status %s\n#+END\n\n"
              (my-kitsu--section-title (format "%s" (cdr sec)))
              (car sec) (cdr sec)))
    my-desktop-kitsu-dashboard-sections)))

(defun my-kitsu--open-file (file skeleton)
  "Open FILE, creating it with SKELETON when missing.  Return buffer."
  (unless (file-exists-p file)
    (make-directory (file-name-directory (expand-file-name file)) t)
    (with-temp-file file (insert skeleton)))
  (let ((buf (find-file-noselect file)))
    (unless noninteractive (switch-to-buffer buf))
    buf))

(defun my-kitsu--setup-local-keys (buf)
  "Add kitsu convenience bindings to org BUF."
  (with-current-buffer buf
    (local-set-key (kbd "C-c C-k") #'my-kitsu-entry-menu)
    (setq-local completion-at-point-functions
                (cons #'my-kitsu--category-cap
                      (remove #'my-kitsu--category-cap
                              completion-at-point-functions)))))

(defun my-kitsu-dashboard ()
  "Open the Kitsu dashboard and refresh every block in it."
  (interactive)
  (message "Kitsu: refreshing dashboard...")
  (let ((buf (my-kitsu--open-file my-desktop-kitsu-org-file
                                  (my-kitsu--dashboard-skeleton))))
    (with-current-buffer buf
      (org-with-point-at 1 (org-update-all-dblocks)))
    (my-kitsu--setup-local-keys buf)
    (message "Kitsu: dashboard ready")
    buf))

(defun my-kitsu-search ()
  "Open the Kitsu search file (saved queries as dynamic blocks).
Run a query with C-c C-c on its block.  Category slugs complete
with M-x completion-at-point on :categories lines."
  (interactive)
  (let ((buf (my-kitsu--open-file
              my-desktop-kitsu-search-file
              "#+TITLE: Kitsu search

# Every block is a saved query; C-c C-c on a block runs it.
# Params on the #+BEGIN line:
#   :q text        required; encode spaces as + (e.g. :q one+piece)
#   :kind          manga or anime (default manga)
#   :categories    comma-separated slugs, complete with M-Tab
#   :min-rating    0-100, client-side filter on averageRating
#   :sort          -userCount (default) or -averageRating
#   :limit         how many results (default 20)
#+BEGIN: kitsu-search :q dungeon+meshi :kind manga :limit 10
#+END
")))
    (my-kitsu--setup-local-keys buf)
    buf))

(defun my-kitsu-trending (kind period)
  "Show a Kitsu trending chart for KIND (manga|anime).
PERIOD is weekly (kitsu.app trending now, ~10 items) or all
(all-time top-N by user count)."
  (interactive
   (list (completing-read "Kind: " '("manga" "anime") nil t "manga")
         (completing-read "Period (weekly/all): " '("weekly" "all")
                          nil t "weekly")))
  (let* ((file my-desktop-kitsu-trending-file)
         (block (format "#+BEGIN: kitsu-trending :kind %s :period %s :limit %d"
                        kind period my-desktop-kitsu-trending-size))
         (buf (my-kitsu--open-file
               file
               (format "#+TITLE: Kitsu trending

# weekly = kitsu.app \"trending now\" (about 10 items);
# all    = all-time top N by user count (server side, paginated).
# Edit the params below and press C-c C-c on the block.
%s
#+END
" block))))
    (message "Kitsu: fetching trending (%s, %s)..." kind period)
    (with-current-buffer buf
      (goto-char (point-min))
      (if (re-search-forward "^#\\+BEGIN: kitsu-trending\\(.*\\)$" nil t)
          (replace-match block t t)
        (goto-char (point-max))
        (insert "\n" block "\n#+END\n"))
      (beginning-of-line)
      (forward-line 1)
      (my-kitsu--refresh-dblock))
    (my-kitsu--setup-local-keys buf)
    (message "Kitsu: trending ready")
    buf))

(defun my-kitsu-bump+1 ()
  "Bump progress of the entry at point by +1."
  (interactive)
  (my-kitsu-bump 1))

(defun my-kitsu-bump-1 ()
  "Bump progress of the entry at point by -1."
  (interactive)
  (my-kitsu-bump -1))

(defun my-kitsu-mark-completed ()
  "Mark the entry at point completed."
  (interactive)
  (my-kitsu-set-status "completed"))

(defun my-kitsu-mark-current ()
  "Mark the entry at point as reading now (current)."
  (interactive)
  (my-kitsu-set-status "current"))

(defun my-kitsu-mark-on-hold ()
  "Put the entry at point on hold."
  (interactive)
  (my-kitsu-set-status "on_hold"))

(defun my-kitsu-mark-dropped ()
  "Mark the entry at point dropped."
  (interactive)
  (my-kitsu-set-status "dropped"))

(transient-define-prefix my-kitsu-entry-menu ()
  "Actions on the Kitsu entry at point."
  [["Progress"
    ("+" "Bump +1" my-kitsu-bump+1)
    ("-" "Bump -1" my-kitsu-bump-1)
    ("n" "Set progress..." my-kitsu-set-progress)]
   ["Status"
    ("c" "Mark completed" my-kitsu-mark-completed)
    ("s" "Reading now" my-kitsu-mark-current)
    ("h" "On hold" my-kitsu-mark-on-hold)
    ("d" "Dropped" my-kitsu-mark-dropped)]
   ["Other"
    ("w" "Open on kitsu.app" my-kitsu-open-web)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix my-kitsu-menu ()
  "Kitsu tracker menu."
  [["Library"
    ("d" "Dashboard" my-kitsu-dashboard)]
   ["Discover"
    ("s" "Search" my-kitsu-search)
    ("t" "Trending..." my-kitsu-trending)]])

(provide 'desktop-kitsu)
;;; desktop-kitsu.el ends here
