;;; desktop-mas-tests.el --- ERT tests of the multi-agent engine -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives `desktop-mas' flows end to end with fake agents: agent-shell,
;; the launchers and podman are stubbed, each "agent" answers a prompt
;; by writing its result file and firing `turn-complete'.  No
;; containers, no network.
;;
;;   emacs --batch -L modules -l ert -l tests/desktop-mas-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; agent-shell is not needed: the module only reaches it through the
;; stubbed functions.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The tests redefine `completing-read'; with native compilation that
;; needs trampolines, which batch runs may fail to build.
(setq native-comp-enable-subr-trampolines nil)

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "../modules"
                                 (file-name-directory
                                  (or load-file-name buffer-file-name
                                      default-directory)))))

;; desktop-dsh-acp installs agent-shell when it is missing; tests must
;; not touch the network, so pretend it is installed and skip loading.
(eval-and-compile
  (cl-letf (((symbol-function 'package-installed-p) (lambda (&rest _) nil))
            ((symbol-function 'package-refresh-contents) #'ignore)
            ((symbol-function 'package-install)
             (lambda (&rest _) (error "No network in tests"))))
    (require 'desktop-mas)))

(defvar my-mas-test--subs nil "Recorded (BUFFER EVENT FN) subscriptions.")
(defvar my-mas-test--prompts nil "Recorded (BUFFER . TEXT) prompts, newest first.")
(defvar my-mas-test--started nil "Recorded (HARNESS EXTRA-ARGS BUFFER).")

(defun my-mas-test--git (dir &rest args)
  "Run git ARGS in DIR."
  (let ((default-directory dir))
    (apply #'call-process "git" nil nil nil args)))

(defmacro my-mas-test--with-project (&rest body)
  "Run BODY in a fresh git project with the agent machinery stubbed."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "mas-test" t)))
          (my-mas--runs nil)
          (my-mas--pending-sweeps nil)
          (my-mas-test--subs nil)
          (my-mas-test--prompts nil)
          (my-mas-test--started nil)
          (my-desktop-mas-prompts-dir
           (expand-file-name "../mas-prompts"
                             (file-name-directory
                              (locate-library "desktop-mas"))))
          (my-desktop-mas-gates nil)
          (my-desktop-mas-max-iterations 20)
          (my-desktop-mas-stall-rounds 2)
          (my-desktop-mas-stop-agents-on-finish t))
     (my-mas-test--git root "init" "-q")
     (with-temp-file (expand-file-name "README" root) (insert "hello\n"))
     (my-mas-test--git root "add" "README")
     (my-mas-test--git root "-c" "user.email=t@t" "-c" "user.name=t"
                       "commit" "-qm" "init")
     (cl-letf (((symbol-function 'my-dsh-acp--require-agent-shell) #'ignore)
               ((symbol-function 'my-dsh-acp--project-root)
                (lambda (&rest _) root))
               ((symbol-function 'my-mas--podman)
                (lambda () (executable-find "true")))
               ((symbol-function 'my-mas--layout) #'ignore)
               ((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'my-dsh-acp-start-owned)
                (cl-function
                 (lambda (&key harness extra-args buffer-name &allow-other-keys)
                   (let ((buffer (generate-new-buffer buffer-name)))
                     (push (list harness extra-args buffer)
                           my-mas-test--started)
                     buffer))))
               ((symbol-function 'agent-shell-subscribe-to)
                (cl-function
                 (lambda (&key shell-buffer event on-event)
                   (push (list shell-buffer event on-event)
                         my-mas-test--subs))))
               ((symbol-function 'agent-shell-insert)
                (cl-function
                 (lambda (&key text shell-buffer &allow-other-keys)
                   (push (cons shell-buffer text) my-mas-test--prompts)))))
       (unwind-protect
           (progn ,@body)
         (dolist (run my-mas--runs)
           (dolist (entry (my-mas-run-agents run))
             (when (buffer-live-p (cdr entry))
               (kill-buffer (cdr entry)))))
         (delete-directory root t)))))

(defun my-mas-test--fire (run event &optional data)
  "Fire EVENT with DATA on the agent RUN is waiting for."
  (let* ((role (plist-get (my-mas-run-waiting run) :role))
         (buffer (my-mas--agent run role)))
    (dolist (sub my-mas-test--subs)
      (when (and (eq (nth 0 sub) buffer) (eq (nth 1 sub) event))
        (with-current-buffer buffer
          (funcall (nth 2 sub)
                   (list (cons :event event) (cons :data data))))))))

(defun my-mas-test--answer (run result)
  "Let the waiting agent of RUN write RESULT (an alist) and end its turn."
  (let ((waiting (my-mas-run-waiting run)))
    (should waiting)
    (when result
      (with-temp-file (plist-get waiting :output)
        (insert (json-serialize result))))
    (my-mas-test--fire run 'turn-complete '((:stop-reason . "end_turn")))
    ;; `my-mas--on-turn-complete' defers the check by 0.5 s.
    (sleep-for 0.7)))

(defun my-mas-test--step (run)
  "Return the step RUN's agent is working on."
  (plist-get (my-mas-run-waiting run) :step))

(defun my-mas-test--finding (id &optional status title)
  "Return a finding alist."
  `((id . ,id) (severity . "major") (title . ,(or title (concat "t" id)))
    (detail . "d") (status . ,(or status "open"))))

(ert-deftest my-mas-openspec-happy-path ()
  (my-mas-test--with-project
    (let ((my-desktop-mas-gates '(after-plan))
          (run (my-mas-openspec "add a demo")))
      (should (equal (my-mas-test--step run) 'plan))
      (should (string-match-p "add a demo" (cdar my-mas-test--prompts)))
      ;; Launcher options: role, outbox, run label, timeout, engine.
      (let ((args (nth 1 (car (last my-mas-test--started)))))
        (should (member "--role" args))
        (should (member "mas-lead" args))
        (should (member (format "sem.mas-run=%s" (my-mas-run-id run)) args))
        (should (member "--timeout" args))
        (should-not (member "--mount" args)))
      (my-mas-test--answer run '((verdict . "done") (summary . "planned")
                                 (change . "demo") (findings . [])))
      (should (eq (my-mas-run-status run) 'gate))
      (my-mas-continue)
      (should (equal (my-mas-test--step run) 'apply))
      (should (string-match-p "openspec/changes/demo/"
                              (cdar my-mas-test--prompts)))
      (my-mas-test--answer run '((verdict . "done") (summary . "done")
                                 (findings . [])))
      (should (equal (my-mas-test--step run) 'review))
      (my-mas-test--answer
       run `((verdict . "changes_requested") (summary . "fix F1")
             (findings . ,(vector (my-mas-test--finding "F1")))))
      (should (equal (my-mas-test--step run) 'fix))
      (should (= (my-mas-run-n run) 2))
      (should (string-match-p "review-1.json" (cdar my-mas-test--prompts)))
      (my-mas-test--answer
       run `((verdict . "done") (summary . "fixed")
             (findings . ,(vector (my-mas-test--finding "F1" "accepted")))))
      (should (equal (my-mas-test--step run) 'review))
      (my-mas-test--answer run '((verdict . "approve") (summary . "ok")
                                 (findings . [])))
      (should (equal (my-mas-test--step run) 'archive))
      (my-mas-test--answer run '((verdict . "done") (summary . "archived")
                                 (findings . [])))
      (should (eq (my-mas-run-status run) 'done))
      (should (null my-mas--runs))
      (should (= (length (my-mas-run-history run)) 6))
      (should (file-exists-p (my-mas--file run "report.org")))
      (should (file-exists-p (my-mas--file run "run.json")))
      ;; Agents stopped, sweep scheduled.
      (should-not (seq-some #'buffer-live-p
                            (mapcar #'cdr (my-mas-run-agents run))))
      (should (member (my-mas-run-id run) my-mas--pending-sweeps))
      ;; .mas/ is ignored through .git/info/exclude.
      (should (string-match-p "/\\.mas/"
                              (my-mas--read-file
                               (expand-file-name ".git/info/exclude" root)))))))

(ert-deftest my-mas-iteration-limit ()
  (my-mas-test--with-project
    (let* ((my-desktop-mas-max-iterations 2)
           (my-desktop-mas-stall-rounds nil)
           (run (my-mas-openspec "demo")))
      (my-mas-test--answer run '((verdict . "done") (summary . "p")
                                 (change . "demo") (findings . [])))
      (my-mas-test--answer run '((verdict . "done") (summary . "a")
                                 (findings . [])))
      (dotimes (i 2)
        (let ((id (format "F%d" i)))
          (my-mas-test--answer
           run `((verdict . "changes_requested") (summary . "r")
                 (findings . ,(vector (my-mas-test--finding id)))))
          (when (eq (my-mas-run-status run) 'running)
            (my-mas-test--answer
             run `((verdict . "done") (summary . "f")
                   (findings . ,(vector (my-mas-test--finding
                                         id "accepted"))))))))
      (should (eq (my-mas-run-status run) 'limit))
      (should (= (my-mas-run-n run) 2)))))

(ert-deftest my-mas-stall-stops-the-run ()
  (my-mas-test--with-project
    (let ((run (my-mas-architecture "cache layer" "docs/design/cache.md")))
      (should (equal (my-mas-test--step run) 'draft))
      ;; The critic mounts the project read-only.
      (let ((critic (seq-find (lambda (s) (eq (car s) 'dsh))
                              my-mas-test--started)))
        (should (member "ro" (nth 1 critic))))
      (my-mas-test--answer run '((verdict . "done") (summary . "d")
                                 (findings . [])))
      (dotimes (_ 3)
        (my-mas-test--answer
         run `((verdict . "changes_requested") (summary . "no")
               (findings . ,(vector (my-mas-test--finding "F1")))))
        (when (eq (my-mas-run-status run) 'running)
          (my-mas-test--answer
           run `((verdict . "done") (summary . "keep")
                 (findings . ,(vector (my-mas-test--finding
                                       "F1" "rejected")))))))
      (should (eq (my-mas-run-status run) 'stalled)))))

(ert-deftest my-mas-architecture-agreement ()
  (my-mas-test--with-project
    (let ((run (my-mas-architecture "cache layer" "docs/design/cache.md")))
      (my-mas-test--answer run '((verdict . "done") (summary . "d")
                                 (findings . [])))
      (my-mas-test--answer
       run `((verdict . "changes_requested") (summary . "r")
             (findings . ,(vector (my-mas-test--finding "F1")
                                  (my-mas-test--finding "F2")))))
      (should (equal (my-mas-test--step run) 'respond))
      (my-mas-test--answer
       run `((verdict . "done") (summary . "a")
             (findings . ,(vector (my-mas-test--finding "F1" "accepted")
                                  (my-mas-test--finding "F2" "rejected")))))
      (should (equal (my-mas-test--step run) 'reroast))
      (should (string-match-p "response-1.json" (cdar my-mas-test--prompts)))
      (my-mas-test--answer
       run `((verdict . "approve") (summary . "agreed")
             (findings . ,(vector (my-mas-test--finding "F1" "resolved")
                                  (my-mas-test--finding "F2" "conceded")))))
      (should (eq (my-mas-run-status run) 'done))
      (should (= (my-mas-run-n run) 2)))))

(ert-deftest my-mas-unanswered-findings-remind-then-pause ()
  (my-mas-test--with-project
    (let ((run (my-mas-architecture "x" "docs/x.md")))
      (my-mas-test--answer run '((verdict . "done") (summary . "d")
                                 (findings . [])))
      (my-mas-test--answer
       run `((verdict . "changes_requested") (summary . "r")
             (findings . ,(vector (my-mas-test--finding "F1")
                                  (my-mas-test--finding "F2")))))
      (let ((partial `((verdict . "done") (summary . "a")
                       (findings . ,(vector (my-mas-test--finding
                                             "F1" "accepted"))))))
        (my-mas-test--answer run partial)
        ;; One reminder naming the missing id, same step.
        (should (eq (my-mas-run-status run) 'running))
        (should (equal (my-mas-test--step run) 'respond))
        (should (string-match-p "missing ids: F2" (cdar my-mas-test--prompts)))
        (my-mas-test--answer run partial)
        (should (eq (my-mas-run-status run) 'paused))
        (should (string-match-p "F2" (my-mas-run-reason run)))))))

(ert-deftest my-mas-reviewer-edits-pause-the-run ()
  (my-mas-test--with-project
    (let ((run (my-mas-openspec "demo")))
      (my-mas-test--answer run '((verdict . "done") (summary . "p")
                                 (change . "demo") (findings . [])))
      (my-mas-test--answer run '((verdict . "done") (summary . "a")
                                 (findings . [])))
      (should (equal (my-mas-test--step run) 'review))
      ;; The reviewer edits a tracked file during its turn.
      (with-temp-file (expand-file-name "README" root) (insert "edited\n"))
      (my-mas-test--answer run '((verdict . "approve") (summary . "ok")
                                 (findings . [])))
      (should (eq (my-mas-run-status run) 'paused))
      (should (string-match-p "README" (my-mas-run-reason run)))
      ;; Continue keeps going with the verdict.
      (my-mas-continue)
      (should (equal (my-mas-test--step run) 'archive)))))

(ert-deftest my-mas-implementer-blockers-go-to-the-lead ()
  (my-mas-test--with-project
    (let ((run (my-mas-openspec "demo")))
      (my-mas-test--answer run '((verdict . "done") (summary . "p")
                                 (change . "demo") (findings . [])))
      (should (equal (my-mas-test--step run) 'apply))
      ;; The implementer may not block: one reminder, same step.
      (my-mas-test--answer run `((verdict . "blocked") (summary . "stuck")
                                 (findings . ,(vector (my-mas-test--finding
                                                       "F1")))))
      (should (eq (my-mas-run-status run) 'running))
      (should (equal (my-mas-test--step run) 'apply))
      (should (string-match-p "\"verdict\" must be one of done"
                              (cdar my-mas-test--prompts)))
      ;; The blocker reported as a finding reaches the lead's review.
      (my-mas-test--answer run `((verdict . "done") (summary . "stuck")
                                 (findings . ,(vector (my-mas-test--finding
                                                       "F1")))))
      (should (equal (my-mas-test--step run) 'review))
      ;; The lead amends the change's design: not a tamper pause.
      (let ((design (expand-file-name "openspec/changes/demo/design.md"
                                      root)))
        (make-directory (file-name-directory design) t)
        (with-temp-file design (insert "decided\n")))
      (my-mas-test--answer
       run `((verdict . "changes_requested") (summary . "decided F1")
             (findings . ,(vector (my-mas-test--finding "F1")))))
      (should (eq (my-mas-run-status run) 'running))
      (should (equal (my-mas-test--step run) 'fix)))))

(ert-deftest my-mas-non-end-turn-pauses-and-retries ()
  (my-mas-test--with-project
    (let ((run (my-mas-openspec "demo")))
      (my-mas-test--fire run 'turn-complete '((:stop-reason . "cancelled")))
      (should (eq (my-mas-run-status run) 'paused))
      (let ((sent (length my-mas-test--prompts)))
        (my-mas-continue)
        (should (= (length my-mas-test--prompts) (1+ sent)))
        (should (equal (my-mas-test--step run) 'plan))))))

(ert-deftest my-mas-killed-agent-aborts-and-stops-the-other ()
  (my-mas-test--with-project
    (let* ((run (my-mas-openspec "demo"))
           (lead (my-mas--agent run 'lead))
           (impl (my-mas--agent run 'impl)))
      ;; What agent-shell does when a buffer is killed.
      (dolist (sub my-mas-test--subs)
        (when (and (eq (nth 0 sub) impl) (eq (nth 1 sub) 'clean-up))
          (with-current-buffer impl
            (funcall (nth 2 sub) '((:event . clean-up))))))
      (kill-buffer impl)
      (sleep-for 0.2)
      (should (eq (my-mas-run-status run) 'aborted))
      (should-not (buffer-live-p lead))
      (should (null my-mas--runs)))))

(ert-deftest my-mas-roles-are-chosen-per-run ()
  (my-mas-test--with-project
    ;; Swapped: DSH leads, Claude implements.
    (let ((run (my-mas-openspec "demo" '((lead . dsh) (impl . claude)))))
      (should (eq (my-mas--role-harness run 'lead) 'dsh))
      (let ((lead (seq-find (lambda (s) (member "mas-lead" (nth 1 s)))
                            my-mas-test--started)))
        (should (eq (car lead) 'dsh)))
      ;; The setting is untouched; the next run uses it again.
      (should (eq (alist-get 'lead (alist-get 'openspec my-desktop-mas-roles))
                  'claude))
      (my-mas--finish run 'aborted "test" t))))

(ert-deftest my-mas-same-harness-in-both-roles ()
  (my-mas-test--with-project
    (let ((run (my-mas-architecture "x" "docs/x.md"
                                    '((architect . claude) (critic . claude)))))
      (should (equal (mapcar #'car my-mas-test--started) '(claude claude)))
      ;; Separate role state: distinct --role values.
      (should (member "mas-architect" (nth 1 (nth 1 my-mas-test--started))))
      (should (member "mas-critic" (nth 1 (nth 0 my-mas-test--started))))
      (should (eq (my-mas-run-status run) 'running)))))

(ert-deftest my-mas-unknown-harness-is-rejected ()
  (my-mas-test--with-project
    (should-error (my-mas-openspec "demo" '((lead . nope) (impl . dsh)))
                  :type 'user-error)
    (should (null my-mas--runs))))

(ert-deftest my-mas-read-roles-defaults-to-the-setting ()
  (let ((asked nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt _choices &rest args)
                 (push prompt asked)
                 ;; RET on the default.
                 (nth 4 args))))
      (should (equal (my-mas--read-roles 'openspec)
                     '((lead . claude) (impl . dsh))))
      (should (string-match-p "lead plays.*Claude" (cadr asked)))
      (should (string-match-p "impl plays.*DSH" (car asked))))))

(ert-deftest my-mas-one-run-per-project ()
  (my-mas-test--with-project
    (my-mas-openspec "demo")
    (should-error (my-mas-openspec "again") :type 'user-error)))

(ert-deftest my-mas-template-placeholders-resolve ()
  ;; Every placeholder used by the shipped templates is known.
  (my-mas-test--with-project
    (let ((run (my-mas-architecture "x" "docs/x.md")))
      (setf (my-mas-run-params run)
            (append '(("change" . "demo") ("request" . "r") ("task" . "t"))
                    (my-mas-run-params run)))
      (dolist (flow my-mas--flows)
        (setf (my-mas-run-flow run) (car flow))
        (dolist (step (plist-get (cdr flow) :steps))
          (when-let* ((prompt (plist-get (cdr step) :prompt)))
            (should (stringp (my-mas--render
                              (my-mas--template (car flow) prompt)
                              (my-mas--vars run (cdr step)))))))))))

;; ------------------------------------------------------------------
;; Implementer + reviewer (pair)
;; ------------------------------------------------------------------

(defun my-mas-test--chat (run role)
  "Fire an operator chat turn's `turn-complete' on ROLE's agent in RUN."
  (let ((buffer (my-mas--agent run role)))
    (dolist (sub my-mas-test--subs)
      (when (and (eq (nth 0 sub) buffer) (eq (nth 1 sub) 'turn-complete))
        (with-current-buffer buffer
          (funcall (nth 2 sub)
                   '((:event . turn-complete)
                     (:data (:stop-reason . "end_turn")))))))
    (sleep-for 0.7)))

(defun my-mas-test--continue (run &optional key)
  "Run `my-mas-continue' in RUN, answering a choice prompt with KEY."
  (cl-letf (((symbol-function 'read-multiple-choice)
             (lambda (_prompt choices &rest _)
               (or (assq key choices) (error "No choice %c" key)))))
    (should (eq (my-mas--current-run) run))
    (my-mas-continue)))

(defun my-mas-test--result (verdict &rest findings)
  "Return a result alist with VERDICT and FINDINGS."
  `((verdict . ,verdict) (summary . "s") (findings . ,(vconcat findings))))

(defun my-mas-test--pair-to-present (run)
  "Drive pair RUN from the discussion to the plan presentation."
  (my-mas-test--continue run)
  (my-mas-test--answer run (my-mas-test--result "done"))
  (my-mas-test--answer run (my-mas-test--result "approve"))
  (should (eq (my-mas-run-status run) 'discuss))
  (should (eq (my-mas-run-step run) 'present)))

(ert-deftest my-mas-pair-happy-path ()
  (my-mas-test--with-project
    (let* ((run (my-mas-pair "add a cache"))
           (reviewer (seq-find (lambda (s) (member "mas-reviewer" (nth 1 s)))
                               my-mas-test--started))
           (implementer (seq-find (lambda (s)
                                    (member "mas-implementer" (nth 1 s)))
                                  my-mas-test--started)))
      ;; Defaults: DSH implements, Claude reviews through an overlay.
      (should (eq (car implementer) 'dsh))
      (should (eq (car reviewer) 'claude))
      (should (equal (member "--mount" (nth 1 reviewer)) '("--mount" "overlay")))
      (should-not (member "--mount" (nth 1 implementer)))
      (should (string-match-p
               "/\\.mas/[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]-[0-9:]\\{8\\}-pair/\\'"
               (my-mas-run-dir run)))
      ;; Discussion: the prompt carries the task, chat turns are ignored.
      (should (eq (my-mas-run-status run) 'discuss))
      (should (string-match-p "add a cache" (cdar my-mas-test--prompts)))
      (my-mas-test--chat run 'implementer)
      (should (eq (my-mas-run-status run) 'discuss))
      (should (null (my-mas-run-waiting run)))
      ;; Plan phase.
      (my-mas-test--continue run)
      (should (eq (my-mas-test--step run) 'plan))
      (my-mas-test--answer run (my-mas-test--result "done"))
      (should (eq (my-mas-test--step run) 'plan-roast))
      (my-mas-test--answer
       run (my-mas-test--result "changes_requested"
                                (my-mas-test--finding "P1")))
      (should (eq (my-mas-test--step run) 'plan-respond))
      (my-mas-test--answer
       run (my-mas-test--result "done" (my-mas-test--finding "P1" "accepted")))
      (should (eq (my-mas-test--step run) 'plan-reroast))
      (should (= (my-mas-run-n run) 2))
      (my-mas-test--answer
       run (my-mas-test--result "approve" (my-mas-test--finding "P1" "resolved")))
      ;; The operator approves the plan; the code phase counts afresh.
      (should (eq (my-mas-run-step run) 'present))
      (my-mas-test--continue run ?a)
      (should (eq (my-mas-test--step run) 'implement))
      (should (eq (my-mas-run-phase run) 'code))
      (should (= (my-mas-run-n run) 1))
      (my-mas-test--answer run (my-mas-test--result "done"))
      (should (eq (my-mas-test--step run) 'review))
      (my-mas-test--answer
       run (my-mas-test--result "changes_requested"
                                (my-mas-test--finding "C1")))
      (should (eq (my-mas-test--step run) 'fix))
      (my-mas-test--answer
       run (my-mas-test--result "done" (my-mas-test--finding "C1" "rejected")))
      (should (eq (my-mas-test--step run) 'rereview))
      (should (string-match-p "fix-1.json" (cdar my-mas-test--prompts)))
      (my-mas-test--answer
       run (my-mas-test--result "approve" (my-mas-test--finding "C1" "conceded")))
      (should (eq (my-mas-run-status run) 'done))
      (let ((report (my-mas--read-file (my-mas--file run "report.org"))))
        (should (string-match-p "plan 2 / 10, code 2 / 10" report))
        (should (string-match-p "plan.md" report))))))

(ert-deftest my-mas-pair-approve-needs-every-finding-closed ()
  (my-mas-test--with-project
    (let ((run (my-mas-pair "x")))
      (my-mas-test--continue run)
      (my-mas-test--answer run (my-mas-test--result "done"))
      ;; A minor finding left open: the approval is sent back once.
      (my-mas-test--answer
       run (my-mas-test--result
            "approve" `((id . "P1") (severity . "minor") (title . "nit")
                        (status . "open"))))
      (should (eq (my-mas-test--step run) 'plan-roast))
      (should (plist-get (my-mas-run-waiting run) :reminded))
      (should (string-match-p "leaves open findings (P1)"
                              (cdar my-mas-test--prompts))))))

(ert-deftest my-mas-pair-phase-limit ()
  (my-mas-test--with-project
    (let ((run (my-mas-pair "x" nil '(:limits ((plan . 1) (code . 10))))))
      (my-mas-test--continue run)
      (my-mas-test--answer run (my-mas-test--result "done"))
      (my-mas-test--answer
       run (my-mas-test--result "changes_requested"
                                (my-mas-test--finding "P1")))
      (my-mas-test--answer
       run (my-mas-test--result "done" (my-mas-test--finding "P1" "accepted")))
      (should (eq (my-mas-run-status run) 'limit))
      (should (string-match-p "plan review rounds" (my-mas-run-reason run))))))

(ert-deftest my-mas-pair-re-review-of-operator-adjustments ()
  (my-mas-test--with-project
    (let ((run (my-mas-pair "x" nil '(:limits ((plan . 2) (code . 10))))))
      (my-mas-test--pair-to-present run)
      (my-mas-test--continue run ?r)
      (should (eq (my-mas-test--step run) 'plan-amend))
      (should (string-match-p "plan-response-1.json"
                              (plist-get (my-mas-run-waiting run) :output)))
      (my-mas-test--answer
       run (my-mas-test--result "done" (my-mas-test--finding "A1" "accepted")))
      (should (eq (my-mas-test--step run) 'plan-reroast))
      (should (string-match-p "plan-response-1.json" (cdar my-mas-test--prompts)))
      (my-mas-test--answer
       run (my-mas-test--result "approve" (my-mas-test--finding "A1" "resolved")))
      (should (eq (my-mas-run-step run) 'present))
      ;; Rounds used up: re-review is no longer offered.
      (let (offered)
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &rest _)
                     (setq offered (mapcar #'car choices))
                     (assq ?a choices))))
          (my-mas-continue))
        (should (equal offered '(?a ?k))))
      (should (eq (my-mas-test--step run) 'implement)))))

(ert-deftest my-mas-pair-abort-at-the-plan ()
  (my-mas-test--with-project
    (let ((run (my-mas-pair "x")))
      (my-mas-test--pair-to-present run)
      (my-mas-test--continue run ?k)
      (should (eq (my-mas-run-status run) 'aborted))
      (should (null my-mas--runs)))))

(ert-deftest my-mas-pair-project-edits-while-presenting-ask ()
  (my-mas-test--with-project
    (let ((run (my-mas-pair "x")))
      (my-mas-test--pair-to-present run)
      (with-temp-file (expand-file-name "early.el" root) (insert "code\n"))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-error (my-mas-test--continue run ?a) :type 'user-error))
      (should (eq (my-mas-run-status run) 'discuss))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (my-mas-test--continue run ?a))
      (should (eq (my-mas-test--step run) 'implement)))))

(ert-deftest my-mas-pair-plan-step-must-not-code ()
  (my-mas-test--with-project
    (let ((run (my-mas-pair "x")))
      (my-mas-test--continue run)
      (with-temp-file (expand-file-name "early.el" root) (insert "code\n"))
      (my-mas-test--answer run (my-mas-test--result "done"))
      (should (eq (my-mas-run-status run) 'paused))
      (should (string-match-p "early.el" (my-mas-run-reason run))))))

(ert-deftest my-mas-pair-menu-settings-start-the-run ()
  (let ((my-mas--pair-settings nil)
        (my-desktop-mas-pair-models nil)
        (my-desktop-mas-pair-rounds '((plan . 10) (code . 10)))
        (my-desktop-mas-pair-reviewer-mount "overlay")
        started)
    (should (equal (my-mas--pair-get :roles)
                   '((implementer . dsh) (reviewer . claude))))
    (my-mas--pair-put :models '((reviewer . "m1")))
    (my-mas-pair-toggle-mount)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "m2"))
              ((symbol-function 'read-number) (lambda (&rest _) 3)))
      (my-mas-pair-set-implementer-model)
      (my-mas-pair-set-code-rounds))
    ;; A new harness drops the role's model.
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Codex")))
      (my-mas-pair-set-reviewer))
    (cl-letf (((symbol-function 'my-mas-pair)
               (lambda (&rest args) (setq started args))))
      (my-mas-pair-start))
    (should (equal (nth 1 started) '((implementer . dsh) (reviewer . codex))))
    (should (equal (plist-get (nth 2 started) :options)
                   '((implementer ("model" . "m2")))))
    (should (equal (plist-get (nth 2 started) :mounts) '((reviewer . "ro"))))
    (should (equal (plist-get (nth 2 started) :limits)
                   '((plan . 10) (code . 3))))
    ;; The configured defaults are untouched; D brings them back.
    (should (equal my-desktop-mas-pair-rounds '((plan . 10) (code . 10))))
    (my-mas-pair-reset)
    (should (equal (my-mas--pair-get :mount) "overlay"))))

(ert-deftest my-mas-acp-config-options-merge ()
  (should (equal (my-dsh-acp--merge-options
                  '(("model" . "a") ("effort" . "high"))
                  '(("model" . "b")))
                 '(("model" . "b") ("effort" . "high"))))
  (should (equal (my-dsh-acp--merge-options '(("effort" . "high"))
                                            '(("model" . "b")))
                 '(("model" . "b") ("effort" . "high"))))
  (should (equal (my-dsh-acp--merge-options nil nil) nil)))

(provide 'desktop-mas-tests)
;;; desktop-mas-tests.el ends here
