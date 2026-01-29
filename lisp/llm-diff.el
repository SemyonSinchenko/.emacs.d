;;; llm-diff.el --- Custom Git Diff for LLM context  -*- lexical-binding: t; -*-

(require 'project) ;; Нам нужна библиотека project.el

(defvar my/llm-diff-ignore-files
  '("uv.lock" "poetry.lock" "package-lock.json" "yarn.lock" "*.svg" "assets/*")
  "Список шаблонов файлов, которые по умолчанию будут игнорироваться в diff.")

(defun my/llm-smart-diff (target-ref ignore-string)
  "Интерактивная функция для получения чистого diff'а (PR или ветка) для LLM.
   TARGET-REF: номер PR (число) или имя ветки.
   IGNORE-STRING: список паттернов для игнорирования через пробел."
  (interactive
   (list
    (read-string "Diff against (PR # or Branch): " "main")
    (read-string "Exclude files: " (mapconcat #'identity my/llm-diff-ignore-files " "))))

  (let* ((default-directory (project-root (project-current t)))
         (buffer-name "*gptel-diff-content*")
         ;; Разбиваем строку игнора и форматируем для git (':!file')
         (ignore-list (split-string ignore-string " " t))
         (pathspecs (mapconcat (lambda (f) (format "':!%s'" f)) ignore-list " "))
         (cmd ""))
    
    (message "Preparing diff for %s..." target-ref)

    ;; ЛОГИКА ВЫБОРА КОМАНДЫ
    (if (string-match-p "^[0-9]+$" target-ref)
        ;; ВАРИАНТ 1: Если введен номер PR (числа)
        (progn
          ;; 1. Скачиваем содержимое PR в специальную ссылку FETCH_HEAD
          ;; (Это не меняет ваши файлы, просто скачивает объекты git)
          (message "Fetching PR #%s data..." target-ref)
          (shell-command (format "git fetch origin pull/%s/head" target-ref))
          
          ;; 2. Сравниваем ваш текущий HEAD с скачанным PR (FETCH_HEAD)
          ;; Три точки (...) найдут общего предка, что идеально для ревью.
          (setq cmd (format "git diff HEAD...FETCH_HEAD -- . %s" pathspecs)))

      ;; ВАРИАНТ 2: Если введена ветка (например, main)
      ;; Сравниваем указанную ветку с нашей текущей (HEAD)
      (setq cmd (format "git diff %s...HEAD -- . %s" target-ref pathspecs)))

    ;; ГЕНЕРАЦИЯ БУФЕРА
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Context: Diff generated from '%s' ignoring [%s]\n\n" target-ref ignore-string))
        (insert "--------------------------------------------------\n")
        ;; Запускаем git и вставляем результат
        (insert (shell-command-to-string cmd))
        
        (diff-mode)
        (read-only-mode 1)
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))
    
    (message "Diff ready in %s" buffer-name)))

(provide 'llm-diff)
;;; llm-diff.el ends here
