;;; desktop-reader.el --- Reading: nov.el now, emacs-reader when built -*- lexical-binding: t; -*-

;;; Commentary:
;; - EPUB via nov.el (with saved reading positions).
;; - emacs-reader (CBZ/FB2/PDF/MOBI/EPUB via MuPDF) is wired up
;;   automatically once it is installed; see
;;   https://codeberg.org/MonadicSheep/emacs-reader
;; - my-reader-log appends a reading-log entry to an Org file.
;; - my-reader-open-manga opens the manga directory in Dired.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

;; emacs-reader: wire it up only when present, never fail otherwise.
(when my-desktop-enable-reader
  (when my-desktop-reader-dir
    (add-to-list 'load-path
                 (expand-file-name my-desktop-reader-dir)))
  (if (locate-library "reader")
      (progn
        (require 'reader)
        ;; Positions persist across restarts through standard
        ;; save-place; the reader package ships the integration.
        (require 'saveplace)
        (setq-default save-place t)
        ;; Auto-append to the Org reading log on document close.
        (when (and my-desktop-reading-log-auto
                   my-desktop-reading-log-file)
          (add-hook 'reader-mode-hook
                    (lambda ()
                      (add-hook 'kill-buffer-hook
                                #'my-reader-log nil t)))))
    (my-desktop--warn
     "emacs-reader not found; CBZ/FB2/PDF fall back to built-in \
modes. Either build it (clone \
https://codeberg.org/MonadicSheep/emacs-reader, run \"make all\", \
needs mupdf >= 1.26 headers) and set my-desktop-reader-dir, or \
install via package-vc on Emacs 30.1+")))

(when my-desktop-reader-mode-alist
  (dolist (entry my-desktop-reader-mode-alist)
    (add-to-list 'auto-mode-alist entry)))

(defun my-reader-open-manga ()
  "Open the manga directory in Dired."
  (interactive)
  (let ((dir (or (my-desktop--expand my-desktop-manga-dir)
                 (read-directory-name "Manga directory: "))))
    (dired dir)))

(defun my-reader-log ()
  "Append a reading-log entry for the current buffer to the log file."
  (interactive)
  (unless my-desktop-reading-log-file
    (user-error "Reading log disabled: set my-desktop-reading-log-file"))
  (let* ((file (expand-file-name my-desktop-reading-log-file))
         (pos (format "%d%%" (/ (* 100 (point)) (max (point-max) 1))))
         (entry (format "* READ [%s] %s — %s\n"
                        (format-time-string "%Y-%m-%d %a %H:%M")
                        (buffer-name)
                        pos)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (goto-char (point-max))
      (insert entry)
      (write-region (point-min) (point-max) file 'append))
    (message "[desktop] reading log: %s" entry)))

(provide 'desktop-reader)
;;; desktop-reader.el ends here
