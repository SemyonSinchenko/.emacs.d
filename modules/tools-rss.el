;;; tools-rss.el --- AI-powered Daily Digest generator & Elfeed Config -*- lexical-binding: t; -*-

;;; Commentary:
;; Full RSS stack:
;; 1. Elfeed (Reader)
;; 2. Elfeed-Org (Subscription management via Org-mode)
;; 3. AI Digest (Generation of daily summaries)

;;; Code:

(require 'seq)
(require 'subr-x)
;; ВАЖНО: Грузим базу данных явно для headless режима
(require 'elfeed)
(require 'elfeed-db)

;; --- 0. Базовая установка Elfeed + Elfeed-Org ---

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  ;; База данных будет лежать в .emacs.d/elfeed
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  ;; Показывать только непрочитанные и "звездные" за последние 2 недели по дефолту
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  ;; Открывать ссылки в браузере по умолчанию
  (setq elfeed-show-entry-switch 'display-buffer))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  ;; Файл с подписками. Создай его: ~/Nextcloud/ORG/feeds.org
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

;; [ЗАЩИТА 1] Лимит статей с одного источника
(defcustom my-rss-max-entries-per-feed 10
  "Anti-spam limit. Maximum number of entries to take from a single feed per day."
  :type 'integer
  :group 'my-rss)

;; [ЗАЩИТА 2] Лимит символов на вход
(defcustom my-rss-max-input-tokens 200000
  "Hard limit on input characters prompt."
  :type 'integer
  :group 'my-rss)

(defcustom my-rss-categories
  '(("dataengineering" . "Data Engineering")
    ("engineers" . "Engineers")
    ("vendors" . "Vendors")
    ("opensource" . "Open Source")
    ("ai" . "Artificial Intillegence")
    ("nonengineering" . "Non Engineering"))
  "Mapping of Elfeed tags to Digest Sections."
  :type '(alist :key-type string :value-type string)
  :group 'my-rss)

;; --- 2. Работа с базой Elfeed (Сбор данных) ---
(defun my/rss--clean-text (html)
  "Aggressively strip HTML, scripts, and whitespace to save tokens.
Returns a single dense line of text."
  (when html
    (with-temp-buffer
      (insert html)
      
      ;; 1. Сначала вырезаем <script> и <style> целиком (внутри них код, а не текст)
      (goto-char (point-min))
      (while (re-search-forward "<\\(script\\|style\\)[^>]*>\\([\\s\\S]*?\\)</\\1>" nil t)
        (replace-match " " nil nil))

      ;; 2. Вырезаем HTML теги
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t)
        (replace-match " " nil nil))

      ;; 3. Декодируем частые HTML-сущности (чтобы &nbsp; стал пробелом)
      (goto-char (point-min))
      (while (re-search-forward "&nbsp;" nil t) (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward "&amp;" nil t) (replace-match "&" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "&quot;" nil t) (replace-match "\"" nil nil))

      ;; 4. ГЛАВНОЕ: Схлопываем ЛЮБОЕ whitespace (включая \n, \r, табы) в один пробел
      (goto-char (point-min))
      ;; [[:space:]] ловит и обычные пробелы, и табы, и \n, и неразрывные пробелы
      (while (re-search-forward "[[:space:]\n\r]+" nil t)
        (replace-match " " nil nil))

      ;; 5. Трим и жесткая обрезка
      (let ((text (string-trim (buffer-string))))
        (if (> (length text) 2500) ;; Чуть увеличим лимит, т.к. текст стал плотнее
            (concat (substring text 0 2500) "...")
          text)))))

(defun my/rss-collect-entries ()
  "Fetch entries from last 24h, GROUP BY feed, APPLY LIMITS and FILTER TAGS."
  (let ((since-time (time-subtract (current-time) (days-to-time 1)))
        (raw-entries '()))
    ;; ВАЖНО: Убеждаемся, что БД загружена в память
    (elfeed-db-ensure)
    
    (with-elfeed-db-visit (entry feed)
      (let ((date (elfeed-entry-date entry)))
        (when (time-less-p since-time (seconds-to-time date))
          (let* ((title (elfeed-entry-title entry))
                 (link (elfeed-entry-link entry))
                 (feed-title (elfeed-feed-title feed))
                 
                 ;; --- ИЗМЕНЕНИЕ: Обработка тегов ---
                 (all-tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
                 ;; Убираем мусорные теги 'unread' и 'starred', оставляем только смысловые
                 (tags (seq-remove (lambda (tag) 
                                     (member tag '("unread" "starred"))) 
                                   all-tags))
                 ;; ----------------------------------

                 (content-raw (elfeed-deref (elfeed-entry-content entry)))
                 ;; Используем нашу новую агрессивную чистку (убедись, что она обновлена выше)
                 (content (my/rss--clean-text content-raw)))
            (push (list :title title
                        :link link
                        :feed feed-title
                        :date date
                        :tags tags
                        :content content)
                  raw-entries)))))

    ;; Фильтрация и защита (осталась без изменений)
    (thread-last raw-entries
		 (seq-group-by (lambda (x) (plist-get x :feed)))
		 (mapcan (lambda (group)
			   (let* ((entries (cdr group))
				  (sorted (seq-sort-by (lambda (x) (plist-get x :date)) #'> entries)))
			     (seq-take sorted my-rss-max-entries-per-feed)))))))

;; --- 3. Генерация Промпта ---

(defun my/rss--format-entry-for-llm (entry)
  (format "Title: %s\nSource: %s\nLink: %s\nTags: %s\nContent Snippet: %s\n---\n"
          (plist-get entry :title)
          (plist-get entry :feed)
          (plist-get entry :link)
          (string-join (plist-get entry :tags) ", ")
          (or (plist-get entry :content) "No content")))

(defun my/rss--build-prompt (entries)
  (let* ((entries-text (mapconcat #'my/rss--format-entry-for-llm entries "\n"))
         (final-text (if (> (length entries-text) my-rss-max-input-tokens)
                         (substring entries-text 0 my-rss-max-input-tokens)
                       entries-text)))
    (format "Analyze the following RSS entries from the last 24 hours.
Target Audience: Senior Software Engineer.

Task:
1. Group articles by categories: %s.
2. Generate a 'Daily Digest' in Russian.
3. OUTPUT FORMAT: Org-mode.
4. NO IMAGES.
5. Use [[Link][Title]] format for all links.

Structure:
* 🚀 Главное за сутки
* 📂 Категории
  ** Category Name
  - [[Link][Title]] (Source) - 1 sentence summary.
* 💎 Выбор редакции (Top 3 Must Read)
  ** [[Link][Title]]
  :SCORE: 8/10
  :WHY: Reasoning.

Data:
%s"
            (mapconcat #'cdr my-rss-categories ", ")
            final-text)))

;; --- 4. Основная логика (UI) ---

(defun my/get-morning-read ()
  "Open today's digest. If it doesn't exist, generate it."
  (interactive)
  (let* ((dir (expand-file-name my-rss-dir))
         (filename (format-time-string "%Y-%m-%d.org"))
         (filepath (expand-file-name filename dir)))
    
    (unless (file-exists-p dir)
      (make-directory dir t))

    (if (file-exists-p filepath)
        (find-file filepath)
      (if (y-or-n-p "Digest for today doesn't exist. Generate now? (Triggers LLM)")
          (my/rss-generate-digest filepath)
        (message "Aborted.")))))

(defun my/rss-generate-digest (target-path)
  (require 'gptel)
  (message "Fetching RSS entries...")
  (elfeed-db-ensure)
  
  (let ((entries (my/rss-collect-entries)))
    (if (null entries)
        (message "No new entries found in the last 24 hours.")
      (message "Found %d entries (filtered). Thinking..." (length entries))
      
      ;; !!! ИСПРАВЛЕНИЕ: Используем let для установки модели !!!
      (let ((gptel-model my-rss-model))
        (gptel-request (my/rss--build-prompt entries)
          :system "You are a helpful Technical Editor assistant."
          ;; :model здесь НЕ нужен, он берется из let выше
          :callback (lambda (response info)
                      (if (not response)
                          (message "LLM Error: %s" (plist-get info :status))
                        (with-temp-file target-path
                          (insert "#+TITLE: Daily Digest: " (format-time-string "%Y-%m-%d") "\n")
                          (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a]") "\n")
                          (insert "#+STARTUP: showall\n\n")
                          (insert response))
                        (message "Digest generated!")
                        (find-file target-path))))))))

(defun my/debug-rss-prompt ()
  "Debug function: Generates the RSS prompt and shows it in a buffer without sending to AI."
  (interactive)
  (require 'tools-rss) ;; Убеждаемся, что функции загружены
  
  (message "Collecting entries from Elfeed (last 24h)...")
  (let ((entries (my/rss-collect-entries)))
    
    (if (null entries)
        (message "⚠️ No entries found! Did you run (elfeed-update)?")
      
      (message "Found %d entries. Building prompt..." (length entries))
      (let ((prompt (my/rss--build-prompt entries))
            (debug-buffer (get-buffer-create "*RSS-Prompt-Debug*")))
        
        (with-current-buffer debug-buffer
          (erase-buffer)
          ;; Включаем режим markdown или text для подсветки
          (markdown-mode) 
          (insert prompt)
          ;; Перематываем в начало, чтобы видеть системный промпт
          (goto-char (point-min)))
        
        ;; Показываем буфер
        (pop-to-buffer debug-buffer)
        (message "Debug prompt generated! Check buffer *RSS-Prompt-Debug*.")))))

(provide 'tools-rss)
;;; tools-rss.el ends here
