;;; desktop-lexicon.el --- Lexicon CLI text transforms for org -*- lexical-binding: t; -*-

;;; Commentary:
;; Local AI text transforms via lexicon-cli, as in the IDE's
;; tools-lexicon.el: region is replaced with an smerge conflict
;; (top = original, bottom = AI result).  Keys live under the C-c l
;; prefix (see modules/desktop-keys.el).  The module is optional: it
;; needs the local checkout from `my-desktop-lexicon-dir'.

;;; Code:

(require 'desktop-config-defs)

(let ((dir (and my-desktop-lexicon-dir
                (expand-file-name my-desktop-lexicon-dir))))
  (cond
   ((not dir))                            ; explicitly disabled
   ((not (file-directory-p dir))
    (my-desktop--warn
     "lexicon: %s not found; AI text transforms (C-c l ...) are \
disabled. Clone the Lexicon repo (it ships emacs/lexicon-org.el) \
and set my-desktop-lexicon-dir, or set \
my-desktop-enable-lexicon to nil" dir))
   (t
    ;; Force the bundled (llama.cpp) backend: local, no Ollama
    ;; needed.  Nil keeps the lexicon-cli default.  Set before first
    ;; use; the CLI reads the environment of the Emacs process.
    (when my-desktop-lexicon-inference
      (setenv "LEXICON_INFERENCE" my-desktop-lexicon-inference))
    (use-package lexicon-org
      :ensure nil                    ; local package, not on MELPA
      :defer t
      :init
      (add-to-list 'load-path dir)
      :commands (lexicon-org-transform
                 lexicon-org-transform-prompt
                 lexicon-org-download
                 lexicon-org-status
                 lexicon-org-remove-last)))))

(provide 'desktop-lexicon)
;;; desktop-lexicon.el ends here
