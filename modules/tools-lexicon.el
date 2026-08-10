;;; tools-lexicon.el --- Lexicon CLI integration for org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Local AI text transforms via lexicon-cli.  Region is replaced with an
;; smerge conflict (top = original, bottom = AI result).

;;; Code:

;; Force the bundled (llama.cpp) backend — faster/local, no Ollama needed.
;; See README: force with env LEXICON_INFERENCE (package has no defcustom
;; for extra CLI flags; it hardcodes "--verbose" + tool args).
(setenv "LEXICON_INFERENCE" "bundled")

(use-package lexicon-org
  :ensure nil                       ; local package, not on MELPA
  :load-path "~/github/Lexicon/emacs"
  :bind (("C-c l t" . lexicon-org-transform)
         ("C-c l p" . lexicon-org-transform-prompt)
         ("C-c l d" . lexicon-org-download)
         ("C-c l s" . lexicon-org-status)
         ("C-c l r" . lexicon-org-remove-last)))

(provide 'tools-lexicon)
;;; tools-lexicon.el ends here
