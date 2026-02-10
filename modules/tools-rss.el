;;; tools-rss.el --- AI-powered Daily Digest generator & Elfeed Config -*- lexical-binding: t; -*-

;;; Commentary:
;; Full RSS stack:
;; 1. Elfeed (Reader)
;; 2. Elfeed-Org (Subscription management via Org-mode)
;; 3. AI Digest (Generation of daily summaries for General news and Arxiv)

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'elfeed)
(require 'elfeed-db)
(require 'gptel)

;; --- 0. Базовая установка Elfeed + Elfeed-Org ---

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-show-entry-switch 'display-buffer))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Nextcloud/ORG/feeds.org")))

;; --- 1. Конфигурация AI Digest ---

(defgroup my-rss nil
  "AI Daily Digest configuration."
  :group 'tools)

(defcustom my-rss-dir "~/Nextcloud/ORG/morning-read/"
  "Directory where daily digests are stored."
  :type 'directory
  :group 'my-rss)

(defcustom my-rss-model 'deepseek/deepseek-v3.2
  "Model to use for summarization."
  :type 'symbol
  :group 'my-rss)

(defcustom my-rss-max-entries-per-feed 10
  "Anti-spam limit. Maximum number of entries to take from a single feed per day."
  :type 'integer
  :group 'my-rss)

(defcustom my-rss-max-input-tokens 200000
  "Hard limit on input characters prompt."
  :type 'integer
  :group 'my-rss)

;; Обычные категории (для my/get-morning-read)
(defcustom my-rss-categories
  '(("dataengineering" . "Data Engineering")
    ("engineers" . "Engineers")
    ("vendors" . "Vendors")
    ("opensource" . "Open Source")
    ("ai" . "Artificial Intillegence")
    ("nonengineering" . "Non Engineering"))
  "Mapping of Elfeed tags to Digest Sections (General)."
  :type '(alist :key-type string :value-type string)
  :group 'my-rss)

;; Arxiv категории (для my/get-morning-arxiv)
(defcustom my-rss-arxiv-categories
  '(("csdb" . "cs.DB (Databases)")
    ("csai" . "cs.AI (Artificial Intelligence)")
    ("socph" . "physics.soc-ph (Social Physics & Networks)")
    ("csds" . "cs.DS (Data Structures & Algorithms)")
    ("csdc" . "cs.DC (Distributed Computing)"))
  "Mapping of Arxiv tags to Digest Sections."
  :type '(alist :key-type string :value-type string)
  :group 'my-rss)

;; --- 2. Работа с базой Elfeed (Сбор данных) ---

(defun my/rss--clean-text (html)
  "Aggressively strip HTML to save tokens."
  (when html
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<\\(script\\|style\\)[^>]*>\\([\\s\\S]*?\\)</\\1>" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward "&nbsp;" nil t) (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward "&amp;" nil t) (replace-match "&" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "&quot;" nil t) (replace-match "\"" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]\n\r]+" nil t)
        (replace-match " " nil nil))
      (let ((text (string-trim (buffer-string))))
        (if (> (length text) 3000) ;; Для Arxiv абстракты могут быть длинными, увеличил лимит
            (concat (substring text 0 3000) "...")
          text)))))

(defun my/rss-collect-entries (filter-fn days)
  "Fetch entries from last DAYS days.
FILTER-FN is a function that takes a list of tag strings and returns non-nil if entry should be kept."
  (let* ((days-int (truncate days))
         (since-time (time-subtract (current-time) (days-to-time days-int)))
         (raw-entries '()))
    (elfeed-db-ensure)
    
    (with-elfeed-db-visit (entry feed)
      (let ((date (elfeed-entry-date entry)))
        (when (time-less-p since-time (seconds-to-time date))
          (let* ((all-tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
                 ;; Убираем системные теги для чистоты
                 (tags (seq-remove (lambda (tag) 
                                     (member tag '("unread" "starred"))) 
                                   all-tags)))
            
            ;; ВАЖНО: Применяем фильтр ДО тяжелой обработки текста
            (when (funcall filter-fn tags)
              (let* ((title (elfeed-entry-title entry))
                     (link (elfeed-entry-link entry))
                     (feed-title (elfeed-feed-title feed))
                     (content-raw (elfeed-deref (elfeed-entry-content entry)))
                     (content (my/rss--clean-text content-raw)))
                (push (list :title title
                            :link link
                            :feed feed-title
                            :date date
                            :tags tags
                            :content content)
                      raw-entries)))))))

    ;; Группировка и лимиты
    (thread-last raw-entries
                 (seq-group-by (lambda (x) (plist-get x :feed)))
                 (mapcan (lambda (group)
                           (let* ((entries (cdr group))
                                  (sorted (seq-sort-by (lambda (x) (plist-get x :date)) #'> entries)))
                             (seq-take sorted my-rss-max-entries-per-feed)))))))

;; --- 3. Генерация Промптов ---

(defun my/rss--format-entry-for-llm (entry)
  (format "Title: %s\nLink: %s\nTags: %s\nAbstract/Content: %s\n---\n"
          (plist-get entry :title)
          (plist-get entry :link)
          (string-join (plist-get entry :tags) ", ")
          (or (plist-get entry :content) "No content")))

(defun my/rss--build-entries-text (entries)
  (let* ((entries-text (mapconcat #'my/rss--format-entry-for-llm entries "\n")))
    (if (> (length entries-text) my-rss-max-input-tokens)
        (substring entries-text 0 my-rss-max-input-tokens)
      entries-text)))

;; -- Промпт для GENERAL (обычные новости) --
(defun my/rss--build-general-prompt (entries days)
  (format "Analyze the following RSS entries from the last %d days.
Target Audience: Senior Software Engineer.
Language: Russian.
Output Format: Org-mode.

Task:
1. Group articles by categories: %s.
2. Structure:
   * 🚀 Главное за %d дней (Executive Summary)
   * 📂 Категории
     ** Category Name
     - [[Link][Title]] (Source) - 1 concise sentence summary.
   * 💎 Выбор редакции (Top 3 interesting reads)
     ** [[Link][Title]]
     :SCORE: X/10
     :WHY: Brief reasoning.
3. Follow ORG-mode convention: * -- top-level header, ** -- sub-header, etc.
4. Result should be org-mode formatted and optimized for reading by human.


Data:
%s"
          days
          (mapconcat #'cdr my-rss-categories ", ")
          days
          (my/rss--build-entries-text entries)))

;; -- Промпт для ARXIV (наука) --
(defun my/rss--build-arxiv-prompt (entries days)
  (format "You are a Research Assistant monitoring Arxiv preprints.
Target Audience: Graph & Data Systems Researcher.
Language: Russian.
Output Format: Org-mode.

Context: The user is interested in Databases, Distributed Systems, and Graph Algorithms/Networks.

Task:
1. Group papers by Arxiv categories: %s.
2. For EACH Category, generate a report with specific subsections (if applicable):

   ** Category Name (e.g. cs.DB)
      *** 🧐 Обзор (Overview of last %d days trend)
      *** 🛠 Практика и Системы (New DBs, DistSys, Optimizations)
          - [[Link][Title]]
            :WHAT: What they built/optimized.
            :IMPACT: Practical value.
      *** 🕸 Графы и Алгоритмы (Graph Algorithms, GNNs, Network Analysis)
          - [[Link][Title]]
            :ALGO: Key algorithmic contribution.
      *** 📄 Остальное (Brief list)
          - [[Link][Title]] - One sentence summary.

3. Ignore purely theoretical papers unless they have clear system applications or graph algorithm breakthroughs.
4. Follow ORG-mode convention: * -- top-level header (Category Name), ** -- sub-header (Обзор, Графы, etc.)
5. Result should be org-mode formatted and optimized for reading by human.

Data:
%s"
          (mapconcat #'cdr my-rss-arxiv-categories ", ")
          days
          (my/rss--build-entries-text entries)))

;; --- 4. Ядро Генерации ---

(defun my/rss--generate-file (target-path prompt title-prefix days)
  "Generic function to call LLM and write to file.
DAYS is the number of days the digest covers."
  (let* ((days-int (truncate days))
         (gptel-model my-rss-model)
         ;; Вычисляем дату начала (DAYS дней назад)
         (from-date (time-subtract (current-time) (days-to-time days-int)))
         ;; Дата окончания - сегодня (включительно)
         (to-date (current-time))
         ;; Форматируем даты в строки
         (from-str (format-time-string "%Y-%m-%d" from-date))
         (to-str (format-time-string "%Y-%m-%d" to-date)))
    (gptel-request prompt
      :system "You are a helpful Technical Editor assistant."
      :callback (lambda (response info)
                  (if (not response)
                      (message "LLM Error: %s" (plist-get info :status))
                    (with-temp-file target-path
                      (insert "#+TITLE: " title-prefix ": " (format-time-string "%Y-%m-%d") "\n")
                      (insert "#+FROM: " from-str "\n")
                      (insert "#+TO: " to-str "\n")
                      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a]") "\n")
                      (insert "#+STARTUP: showall\n\n")
                      (insert response))
                    (message "Digest generated at %s" target-path)
                    (find-file target-path)))))))

(defun my/rss--run-logic (filename-fmt filter-fn prompt-builder-fn title-prefix days)
  "Main logic orchestrator.
DAYS is the number of days to look back."
  (let* ((dir (expand-file-name my-rss-dir))
         (filename (format-time-string filename-fmt))
         (filepath (expand-file-name filename dir)))
    
    (unless (file-exists-p dir) (make-directory dir t))

    (if (file-exists-p filepath)
        (find-file filepath)
      (if (y-or-n-p (format "Generate %s for last %d days (Triggers LLM)?" title-prefix days))
          (progn
            (message "Fetching entries...")
            (elfeed-db-ensure)
            (let ((entries (my/rss-collect-entries filter-fn days)))
              (if (null entries)
                  (message "No entries found for filter.")
                (message "Found %d entries. Asking AI..." (length entries))
                (my/rss--generate-file filepath 
                                       (funcall prompt-builder-fn entries days)
                                       title-prefix
                                       days))))  ;; ← Добавляем передачу дней
        (message "Aborted.")))))

;; --- 5. Интерактивные команды (UI) ---

;;;###autoload
(defun my/get-morning-read ()
  "General Daily Digest (Excludes Arxiv)."
  (interactive)
  (let ((days (read-number "Number of days to include in digest: " 1)))
    (my/rss--run-logic 
     "%Y-%m-%d.org"
     ;; Фильтр: ИСКЛЮЧИТЬ 'arxiv'
     (lambda (tags) (not (member "arxiv" tags)))
     #'my/rss--build-general-prompt
     "Daily Digest"
     days)))

;;;###autoload
(defun my/get-morning-arxiv ()
  "Arxiv Research Digest (Only Arxiv)."
  (interactive)
  (let ((days (read-number "Number of days to include in digest: " 1)))
    (my/rss--run-logic 
     "%Y-%m-%d-arxiv.org"
     ;; Фильтр: ТОЛЬКО 'arxiv'
     (lambda (tags) (member "arxiv" tags))
     #'my/rss--build-arxiv-prompt
     "Arxiv Digest"
     days)))

(provide 'tools-rss)
;;; tools-rss.el ends here
