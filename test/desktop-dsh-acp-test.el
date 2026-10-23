;;; desktop-dsh-acp-test.el --- Tests for DSH transcript parsing -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from the repository root:
;;
;;   emacs --batch -L modules -l test/desktop-dsh-acp-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package)
(setq package-user-dir
      (expand-file-name
       "elpa"
       (file-name-directory
        (directory-file-name
         (file-name-directory (or load-file-name buffer-file-name))))))
(package-initialize)

(require 'ert)
(require 'desktop-dsh-acp)

(defun my-dsh-acp-test--parse (text)
  "Parse TEXT as a transcript and return the session plist."
  (with-temp-buffer
    (insert text)
    (my-dsh-acp--parse-transcript-buffer)))

(ert-deftest my-dsh-acp-test-frontmatter-uuid ()
  "Parse a normal UUID session header and the first user message."
  (let ((session
         (my-dsh-acp-test--parse
          "---
agent: \"DSH ACP\"
started: \"2026-10-23T12:33:17+02:00\"
working_directory: \"/var/home/sem/.emacs.d/\"
session_id: \"38fd0d97-64bc-41d8-aaed-46fdea8a6f8a\"
---

# Agent Shell Transcript

## User (2026-10-23 12:33:17)

What is Databricks?

## Agent (2026-10-23 12:33:21)

Databricks is...
")))
    (should (equal (plist-get session :id)
                   "38fd0d97-64bc-41d8-aaed-46fdea8a6f8a"))
    (should (equal (plist-get session :title) "What is Databricks?"))
    (should (equal (plist-get session :working-directory)
                   "/var/home/sem/.emacs.d/"))
    (should (equal (plist-get session :started)
                   (date-to-time "2026-10-23T12:33:17+02:00")))))

(ert-deftest my-dsh-acp-test-frontmatter-weird-id ()
  "Parse a JSON-encoded id with punctuation, quotes and Unicode."
  (let* ((id "a+b/c=d:e.f_g-h~i!@$%^&*()[]{}|;'\"λ")
         (text (format "---
started: \"2026-10-23T12:33:17+02:00\"
session_id: %s
---

## User (2026-10-23 12:33:17)

weird id
"
                       (json-encode-string id)))
         (session (my-dsh-acp-test--parse text)))
    (should (equal (plist-get session :id) id))))

(ert-deftest my-dsh-acp-test-clean-pasted-session-id ()
  "Extract an id from a pasted transcript header line."
  (should (equal (my-dsh-acp--clean-session-id "  \"abc+def\"  ")
                 "abc+def"))
  (should (equal (my-dsh-acp--clean-session-id
                  "session_id: \"a+b/c=d:e.f_g-h~i\"")
                 "a+b/c=d:e.f_g-h~i"))
  (should (equal (my-dsh-acp--clean-session-id
                  "session_id: \"a\\\"b\\\\c\"")
                 "a\"b\\c")))

(ert-deftest my-dsh-acp-test-session-id-at-point ()
  "Read the id from a transcript header line at point."
  (with-temp-buffer
    (insert "session_id: \"a+b/c=d:e.f_g-h~i\"\n")
    (goto-char (point-min))
    (should (equal (my-dsh-acp--session-id-at-point)
                   "a+b/c=d:e.f_g-h~i"))))

(ert-deftest my-dsh-acp-test-frontmatter-without-id ()
  "A transcript without session_id is not resumable."
  (should-not
   (my-dsh-acp-test--parse
    "---
started: \"2026-10-23T12:33:17+02:00\"
---

## User (2026-10-23 12:33:17)

hello
")))

(provide 'desktop-dsh-acp-test)
;;; desktop-dsh-acp-test.el ends here