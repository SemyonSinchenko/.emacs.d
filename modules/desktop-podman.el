;;; desktop-podman.el --- Podman helpers under C-c p -*- lexical-binding: t; -*-

;;; Commentary:
;; Small podman helpers for the dev-container agents.
;;
;; `my-podman-pick-container' (C-c p l) lists the running containers
;; in the minibuffer, one per line (ID, name, image, status, and the
;; sem.role / sem.mas-run labels the launchers set).  Up/Down or
;; C-n/C-p move, RET copies the full container ID to the kill ring and
;; closes the list, C-g closes it.  Useful to stop a leftover agent
;; container from a terminal: `podman container stop <ID>'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'desktop-config-defs)

(defun my-podman--program ()
  "Return the podman executable, or signal a `user-error'."
  (or (executable-find my-desktop-podman-command)
      (user-error "Podman: `%s' not found on `exec-path'"
                  my-desktop-podman-command)))

(defun my-podman--running-containers ()
  "Return the running containers as a list of alists (podman JSON)."
  (let ((program (my-podman--program)))
    (with-temp-buffer
      (let ((status (call-process program nil t nil
                                  "ps" "--format" "json")))
        (unless (eq status 0)
          (user-error "Podman: `podman ps' failed: %s"
                      (string-trim (buffer-string))))
        (goto-char (point-min))
        (if (looking-at-p "[[:space:]]*\\'")
            nil
          (json-parse-buffer :object-type 'alist :array-type 'list
                             :null-object nil :false-object nil))))))

(defun my-podman--label (container key)
  "Return label KEY of CONTAINER, or an empty string."
  (or (alist-get (intern key) (alist-get 'Labels container)) ""))

(defun my-podman--row (container)
  "Return the display fields of CONTAINER as a list of strings."
  (let ((names (alist-get 'Names container)))
    (list (substring (or (alist-get 'Id container) "") 0
                     (min 12 (length (or (alist-get 'Id container) ""))))
          (if (listp names) (string-join names ",") (or names ""))
          (or (alist-get 'Image container) "")
          (or (alist-get 'Status container)
              (alist-get 'State container) "")
          (my-podman--label container "sem.role")
          (my-podman--label container "sem.mas-run"))))

(defun my-podman--format-rows (rows)
  "Align ROWS (lists of strings) into columns; return the lines."
  (let ((widths (apply #'seq-mapn
                       (lambda (&rest cells)
                         (apply #'max (mapcar #'string-width cells)))
                       rows)))
    (mapcar (lambda (row)
              (string-trim-right
               (string-join (seq-mapn (lambda (cell width)
                                        (string-pad cell width))
                                      row widths)
                            "  ")))
            rows)))

;;;###autoload
(defun my-podman-pick-container ()
  "Pick a running podman container and copy its full ID to the kill ring.
The list shows one container per line; RET copies the ID and closes
it, C-g closes it without copying."
  (interactive)
  (let ((containers (my-podman--running-containers)))
    (if (null containers)
        (message "Podman: no running containers")
      (let* ((lines (my-podman--format-rows
                     (mapcar #'my-podman--row containers)))
             (choices (seq-mapn #'cons lines containers))
             ;; Keep podman's order (newest first); Vertico would sort.
             (completion-extra-properties
              '(:display-sort-function identity
                :cycle-sort-function identity))
             (choice (completing-read
                      "Container (RET copies the ID): " choices nil t))
             (id (alist-get 'Id (cdr (assoc choice choices)))))
        (when id
          (kill-new id)
          (message "Podman: copied container ID %s" id))))))

(provide 'desktop-podman)
;;; desktop-podman.el ends here
