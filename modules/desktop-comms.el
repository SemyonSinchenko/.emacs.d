;;; desktop-comms.el --- Telegram (telega) for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; Telegram in Emacs via telega.
;;
;; telega needs a `telega-server' process built against TDLib.  Two
;; ways, chosen by `my-desktop-telega-docker':
;;
;; - nil (default, native): install TDLib with development files,
;;   then build the server once: M-x telega-server-build
;;
;; - t (docker): no local TDLib needed -- telega runs its server in
;;   the zevlg/telega-server container.  Requires the `docker' CLI
;;   (podman users: sudo dnf install podman-docker for the alias).
;;
;; Either way: set my-desktop-enable-telega to t, restart, then
;; C-c M-t and log in once (telega stores the session under
;; telega-directory).
;;
;; Emoji note: svg.el encodes non-BMP emoji as XML numeric character
;; references and librsvg renders those as black boxes.  The advice
;; below decodes them back to literal UTF-8 in every emoji image
;; telega builds, so they render colored.

;;; Code:

(require 'desktop-config-defs)
(require 'desktop-core)

;; ------------------------------------------------------------------
;; Emoji SVG fix: decode NCRs that librsvg cannot render
;; ------------------------------------------------------------------

(defun my-telega--decode-svg-ncrs (data)
  "Decode XML numeric character references in DATA to UTF-8 text,
combining UTF-16 surrogate pairs (svg.el emits emoji as pairs of
NCRs, which librsvg cannot handle)."
  (let ((pos 0) (out ""))
    (while (< pos (length data))
      (if (and (eq (aref data pos) ?&)
               (string-match "&#\\([0-9]+\\);" data pos)
               (= (match-beginning 0) pos))
          (let (cps)
            (while (and (< pos (length data))
                        (eq (aref data pos) ?&)
                        (string-match "&#\\([0-9]+\\);" data pos)
                        (= (match-beginning 0) pos))
              (push (string-to-number (match-string 1 data)) cps)
              (setq pos (match-end 0)))
            (setq cps (nreverse cps))
            (let ((i 0))
              (while (< i (length cps))
                (let ((cp (nth i cps)))
                  (when (and (>= cp #xD800) (<= cp #xDBFF)
                             (< (1+ i) (length cps))
                             (>= (nth (1+ i) cps) #xDC00)
                             (<= (nth (1+ i) cps) #xDFFF))
                    (setq cp (+ #x10000 (- cp #xD800)
                                (- (nth (1+ i) cps) #xDC00)))
                    (setq i (1+ i)))
                  (setq out (concat out (char-to-string cp)))
                  (setq i (1+ i))))))
        (setq out (concat out (char-to-string (aref data pos))))
        (setq pos (1+ pos))))
    out))

(defun my-telega--fix-emoji-svg (orig emoji &optional cheight no-cache-p)
  "Decode NCRs in the SVG data telega-emoji-create-svg generates."
  (let ((img (funcall orig emoji cheight no-cache-p)))
    (when (consp img)
      (let ((data (plist-get (cdr img) :data)))
        (when (and data (string-match-p "&#[0-9]+;" data))
          (plist-put (cdr img) :data
                     (my-telega--decode-svg-ncrs data)))))
    img))

;; ------------------------------------------------------------------
;; telega
;; ------------------------------------------------------------------

(when my-desktop-enable-telega
  (use-package telega
    :ensure t
    :commands (telega telega-switch-buffer)
    :init
    ;; Applied before telega loads; defcustoms keep these values.
    (when my-desktop-telega-docker
      (setq telega-use-docker t))
    (when my-desktop-telega-proxies
      (setq telega-proxies (append my-desktop-telega-proxies nil)))
    :config
    (setq telega-use-images t)
    ;; Resolve the emoji font explicitly: color fonts first, then
    ;; monochrome fallbacks (Symbola per the telega FAQ).
    (setq telega-emoji-font-family
          (seq-some (lambda (ef)
                      (car (member ef (font-family-list))))
                    (list "Noto Color Emoji" "EmojiOne" "Emoji One"
                          "Twemoji Mozilla" "Twemoji" "Symbola"
                          "FreeSerif" "GNU Unifont" "Unifont")))
    ;; Text emoji by default (images render as boxes with this
    ;; machine's librsvg); flip my-desktop-telega-emoji-images to t
    ;; if that ever changes.
    (setq telega-emoji-use-images
          (and my-desktop-telega-emoji-images telega-emoji-font-family t))
    (unless telega-emoji-font-family
      (my-desktop--warn
       "No emoji font visible to fontconfig: telega emoji will be black boxes in GUI.  Install an emoji font (google-noto-emoji-fonts / fonts-noto-color-emoji) and restart Emacs."))
    (advice-add 'telega-emoji-create-svg :around
                (function my-telega--fix-emoji-svg))
    ;; Text emoji: route Unicode through Symbola as well (telega FAQ).
    (when (member "Symbola" (font-family-list))
      (set-fontset-font t (quote unicode) "Symbola" nil (quote append))))

  (defun my-telega ()
    "Open Telegram."
    (interactive)
    (telega)))

(provide 'desktop-comms)
;;; desktop-comms.el ends here
