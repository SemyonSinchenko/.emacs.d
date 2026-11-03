;;; desktop-podman-tests.el --- ERT tests of the podman picker -*- lexical-binding: t; -*-

;;; Commentary:
;; `podman ps' and the minibuffer are stubbed; no podman needed.
;;
;;   emacs --batch -L modules -l ert -l tests/desktop-podman-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The tests redefine primitives (call-process, completing-read); with
;; native compilation that needs trampolines, which batch runs may fail
;; to build.  Plain redefinition is enough here.
(setq native-comp-enable-subr-trampolines nil)

(add-to-list 'load-path
             (expand-file-name "../modules"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'desktop-podman)

(defconst my-podman-test--json
  "[{\"Id\":\"aaaaaaaaaaaa1111\",\"Names\":[\"mas-1-lead\"],
     \"Image\":\"localhost/dev-claude:latest\",\"Status\":\"Up 5 minutes\",
     \"Labels\":{\"sem.role\":\"mas-lead\",\"sem.mas-run\":\"1\"}},
    {\"Id\":\"bbbbbbbbbbbb2222\",\"Names\":[\"quirky_name\"],
     \"Image\":\"localhost/dev-dsh:latest\",\"Status\":\"Up 1 hour\",
     \"Labels\":null}]"
  "Sample `podman ps --format json' output.")

(defmacro my-podman-test--with-ps (output &rest body)
  "Run BODY with `podman ps' printing OUTPUT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "podman"))
             ((symbol-function 'call-process)
              (lambda (&rest _) (insert ,output) 0)))
     ,@body))

(ert-deftest my-podman-pick-copies-the-full-id ()
  (my-podman-test--with-ps my-podman-test--json
    (let ((kill-ring nil) (seen nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq seen (mapcar #'car choices))
                   (cadr seen))))
        (my-podman-pick-container))
      ;; One aligned line per container, podman's order kept.
      (should (= (length seen) 2))
      (should (string-prefix-p "aaaaaaaaaaaa  mas-1-lead" (car seen)))
      (should (string-match-p "mas-lead" (car seen)))
      (should (string-prefix-p "bbbbbbbbbbbb  quirky_name" (cadr seen)))
      (should (equal (car kill-ring) "bbbbbbbbbbbb2222")))))

(ert-deftest my-podman-pick-quit-copies-nothing ()
  (my-podman-test--with-ps my-podman-test--json
    (let ((kill-ring nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (signal 'quit nil))))
        ;; C-g signals `quit', which is not an `error'.
        (should (eq (condition-case nil
                        (progn (my-podman-pick-container) 'returned)
                      (quit 'quit))
                    'quit)))
      (should (null kill-ring)))))

(ert-deftest my-podman-pick-no-containers ()
  (my-podman-test--with-ps "[]"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Should not prompt"))))
      (should (equal (my-podman-pick-container)
                     "Podman: no running containers")))))

(provide 'desktop-podman-tests)
;;; desktop-podman-tests.el ends here
