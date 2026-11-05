;;; desktop-mas.el --- Multi-agent runs over the ACP agents (C-c a m) -*- lexical-binding: t; -*-

;;; Commentary:
;; A small, deterministic orchestrator for two agents working on one
;; project.  Emacs drives the loop; the agents (Claude, Codex or DSH,
;; each in its own sem-dev-container container, shown side by side
;; through agent-shell) only do or judge one step at a time.
;;
;; Flows are data (`my-mas--flows'): a list of steps, each run by one
;; role, with transitions on the step's verdict.  Two flows exist:
;;
;;   openspec      lead (Claude) drafts the change, impl (DSH) applies
;;                 it, lead verifies and reviews, impl fixes, ... until
;;                 lead approves; then lead archives.  Nothing is
;;                 committed: commits stay the operator's job.  impl
;;                 never blocks: it reports blockers as open findings,
;;                 and the lead's review decides them (it may amend the
;;                 change's artifacts); only the lead escalates to the
;;                 operator with `blocked'.
;;   architecture  architect (Claude) writes a design, critic (DSH)
;;                 roasts it against the current worktree (read-only
;;                 mount), architect fixes or pushes back, ... until the
;;                 critic approves.
;;   pair          (C-c a m i, a menu of roles, models, mount and rounds)
;;                 you discuss the task with the implementer (DSH), C-c
;;                 a m c starts planning: it writes brief.md + plan.md
;;                 in the run folder, the reviewer (Claude, overlay
;;                 mount: may build and test, writes never reach the
;;                 project) roasts the plan, ... until it approves; the
;;                 implementer presents the plan and C-c a m c asks you:
;;                 approve (coding starts), re-review your adjustments,
;;                 or abort.  Then the implementer codes and the
;;                 reviewer reviews until it approves.  Each phase has
;;                 its own round limit (`my-desktop-mas-pair-rounds');
;;                 the reviewer may approve only when every finding,
;;                 minor ones included, is resolved or conceded.
;;
;; Roles are not tied to a harness: C-c a m o / a ask which ACP agent
;; (Claude, Codex or DSH, see `my-dsh-acp--harnesses') plays each
;; role, with `my-desktop-mas-roles' as the default (the defaults are
;; shown in parentheses above).  Both roles may use the same harness.
;;
;; Steps hand off through files.  Every run has a folder
;; <project>/.mas/<time>-<flow>/ (`my-desktop-mas-run-dir-time-format'),
;; mounted into both containers as the
;; launcher's --outbox; each step must write one JSON result file
;; there (see mas-prompts/schema.md).  A step is complete when the
;; agent's turn ends (`turn-complete', stop reason end_turn) and its
;; file parses and validates; otherwise the agent gets one reminder and
;; then the run pauses.  Only one agent works at a time.
;;
;; Discussion steps (:discuss) hand the agent to you instead: Emacs
;; sends one prompt, ignores the turns of your chat, and goes on when
;; you press C-c a m c.
;;
;; Safeguards: `my-desktop-mas-max-iterations' review rounds (per phase
;; limits in the pair flow), a stall
;; check (`my-desktop-mas-stall-rounds'), gates
;; (`my-desktop-mas-gates'), and in the openspec flow a worktree check
;; that the reviewer did not edit project files.
;;
;; Containers: every agent is started with --role mas-<role>,
;; --label sem.mas-run=<id>, --name mas-<id>-<role> and --timeout
;; `my-desktop-mas-run-timeout'.  When a run ends (or an agent buffer
;; is killed) both agent buffers are killed and any container still
;; labelled with the run is removed with `podman rm -f'; on a normal
;; Emacs exit `kill-emacs-hook' removes them synchronously.  Only an
;; Emacs crash can leave containers behind, and --timeout bounds those.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'desktop-config-defs)
(require 'desktop-dsh-acp)

(declare-function agent-shell-insert "agent-shell" t)
(declare-function agent-shell-subscribe-to "agent-shell" t)
(declare-function notifications-notify "notifications" t)
(declare-function persp-add-buffer "perspective" t)

;; ------------------------------------------------------------------
;; Flows
;; ------------------------------------------------------------------

(defconst my-mas--flows
  '((openspec
     :title "openspec change"
     :mounts ((lead . "rw") (impl . "rw"))
     :start plan
     :steps
     ((plan :role lead :prompt "plan" :output "plan.json"
            :capture (change) :next gate-plan)
      (gate-plan :gate after-plan :next apply)
      (apply :role impl :prompt "apply" :output "impl-{{n}}.json"
             :verdicts ("done") :next review)
      (review :role lead :prompt "review" :critique t :tamper t
              :may-edit "openspec/changes/{{change}}"
              :input "impl-{{n}}.json" :output "review-{{n}}.json"
              :on ((approve . gate-archive) (changes_requested . fix)))
      (fix :role impl :prompt "fix" :bump t :answers t
           :input "review-{{prev_n}}.json" :output "impl-{{n}}.json"
           :verdicts ("done") :next review)
      (gate-archive :gate before-archive :next archive)
      (archive :role lead :prompt "archive" :output "archive.json"
               :next report)
      (report :report t)))
    (architecture
     :title "architecture decision"
     :mounts ((architect . "rw") (critic . "ro"))
     :start draft
     :steps
     ((draft :role architect :prompt "draft" :output "draft.json"
             :next roast)
      (roast :role critic :prompt "roast" :critique t
             :output "roast-{{n}}.json"
             :on ((approve . report) (changes_requested . respond)))
      (respond :role architect :prompt "respond" :answers t
               :input "roast-{{n}}.json" :output "response-{{n}}.json"
               :next reroast)
      (reroast :role critic :prompt "reroast" :critique t :bump t
               :input "response-{{prev_n}}.json"
               :output "roast-{{n}}.json"
               :on ((approve . report) (changes_requested . respond)))
      (report :report t)))
    (pair
     :title "implementer + reviewer"
     :mounts ((implementer . "rw") (reviewer . "overlay"))
     :start discuss
     :steps
     ((discuss :role implementer :prompt "discuss" :discuss t :phase plan
               :next plan)
      (plan :role implementer :prompt "plan" :phase plan :tamper t
            :output "plan.json" :next plan-roast)
      (plan-roast :role reviewer :prompt "plan-roast" :phase plan
                  :critique t :approve-clean t :tamper t
                  :output "plan-roast-{{n}}.json"
                  :on ((approve . present) (changes_requested . plan-respond)))
      (plan-respond :role implementer :prompt "plan-respond" :phase plan
                    :answers t :tamper t
                    :input "plan-roast-{{n}}.json"
                    :output "plan-response-{{n}}.json" :next plan-reroast)
      (plan-reroast :role reviewer :prompt "plan-reroast" :phase plan
                    :critique t :approve-clean t :tamper t :bump t
                    :input "plan-response-{{prev_n}}.json"
                    :output "plan-roast-{{n}}.json"
                    :on ((approve . present)
                         (changes_requested . plan-respond)))
      (present :role implementer :prompt "present" :discuss t :phase plan
               :tamper t
               :choices ((?a "approve" implement "start coding")
                         (?r "re-review" plan-amend
                             "the reviewer checks your adjustments" round)
                         (?k "abort" abort "stop both agents")))
      (plan-amend :role implementer :prompt "plan-amend" :phase plan
                  :tamper t :output "plan-response-{{n}}.json"
                  :next plan-reroast)
      (implement :role implementer :prompt "implement" :phase code
                 :output "impl-{{n}}.json" :next review)
      (review :role reviewer :prompt "review" :phase code
              :critique t :approve-clean t :tamper t
              :input "impl-{{n}}.json" :output "review-{{n}}.json"
              :on ((approve . report) (changes_requested . fix)))
      (fix :role implementer :prompt "fix" :phase code :answers t
           :input "review-{{n}}.json" :output "fix-{{n}}.json"
           :next rereview)
      (rereview :role reviewer :prompt "rereview" :phase code
                :critique t :approve-clean t :tamper t :bump t
                :input "fix-{{prev_n}}.json" :output "review-{{n}}.json"
                :on ((approve . report) (changes_requested . fix)))
      (report :report t))))
  "Multi-agent flows, as (FLOW . PLIST).
:mounts gives each role's project mount (rw, ro or overlay), :start
the first step, :steps the steps as (STEP . PLIST):

  :role     role that runs the step (harness from `my-desktop-mas-roles')
  :prompt   template mas-prompts/<flow>/<prompt>.md
  :output   result file name in the run folder ({{n}} = iteration)
  :input    file the step answers or reviews (optional)
  :critique the step judges work: verdict approve, changes_requested
            or blocked, and :on maps the verdict to the next step;
            other steps report done or blocked and go to :next
  :verdicts the verdicts the step may report, when narrower than the
            above (the openspec implementer never blocks: it reports
            blockers as open findings and the lead's review answers)
  :bump     entering the step starts the next iteration (limit check)
  :answers  the output must answer every open finding of :input
  :tamper   the agent must not change the project worktree
  :may-edit a path (template) exempt from the :tamper check
  :capture  result fields stored as template variables
  :gate     wait for `my-mas-continue' when the gate is in
            `my-desktop-mas-gates'
  :discuss  send the prompt, then let the operator chat with the agent
            (no result file); `my-mas-continue' ends the step and goes
            to :next, or asks among :choices, as (KEY NAME NEXT
            DESCRIPTION [round]): NEXT `abort' aborts the run, `round'
            hides the choice once the phase's rounds are used up
  :phase    the round counter's phase; entering a new phase restarts
            the count, with the phase's own limit (see `my-mas--max')
  :approve-clean  `approve' is accepted only without open findings,
            minor ones included
  :report   write the report and finish")

(defconst my-mas--critique-verdicts '("approve" "changes_requested" "blocked")
  "Verdicts a critique step may report.")

(defconst my-mas--work-verdicts '("done" "blocked")
  "Verdicts a working (non-critique) step may report.")

(defconst my-mas--worktree-excludes
  '(":(exclude).mas" ":(exclude).agent-shell"
    ":(exclude).claude-container" ":(exclude).codex-container"
    ":(exclude).dsh")
  "Pathspecs ignored by the reviewer worktree check.")

(defconst my-mas--remove-delay 10
  "Seconds between killing the agent buffers and `podman rm -f'.
agent-shell sends SIGTERM, then SIGKILL after a grace period; this
leaves time for a clean container exit before forcing it.")

(defun my-mas--flow (flow)
  "Return the plist of FLOW."
  (or (alist-get flow my-mas--flows)
      (error "MAS: unknown flow %s" flow)))

(defun my-mas--flow-roles (flow)
  "Return FLOW's default roles as (ROLE . HARNESS), in display order."
  (or (alist-get flow my-desktop-mas-roles)
      (user-error "MAS: no roles for flow %s in `my-desktop-mas-roles'"
                  flow)))

(defun my-mas--check-roles (flow roles)
  "Signal a `user-error' unless ROLES assigns a known harness to every
role of FLOW; return ROLES."
  (dolist (entry (my-mas--flow-roles flow))
    (let ((harness (alist-get (car entry) roles)))
      (unless (alist-get harness my-dsh-acp--harnesses)
        (user-error "MAS: role %s of %s has no known harness (%S)"
                    (car entry) flow harness))))
  roles)

(defun my-mas--read-roles (flow)
  "Ask which harness plays each role of FLOW; defaults from the setting.
Any harness may play any role, also both roles (they run as separate
containers with separate --role state)."
  (let* ((choices (mapcar (lambda (entry)
                            (cons (plist-get (cdr entry) :name) (car entry)))
                          my-dsh-acp--harnesses))
         (completion-extra-properties '(:display-sort-function identity)))
    (mapcar
     (lambda (entry)
       (let* ((default (my-dsh-acp--harness-name (cdr entry)))
              (choice (completing-read
                       (format-prompt "%s: %s plays" default flow (car entry))
                       choices nil t nil nil default)))
         (cons (car entry) (cdr (assoc choice choices)))))
     (my-mas--flow-roles flow))))

;; ------------------------------------------------------------------
;; Runs
;; ------------------------------------------------------------------

(cl-defstruct (my-mas-run (:constructor my-mas--make-run)
                          (:copier nil))
  id flow roles root dir params status step (n 1) reason pending
  agents waiting history last-critique open-prev (stall 0)
  snapshot pause-requested started idle-timer stopping
  phase limits mounts options)

(defvar my-mas--runs nil
  "Runs whose agents are still alive (one per project).")

(defvar-local my-mas--run nil
  "The run this agent-shell buffer belongs to.")

(defun my-mas--active-p (run)
  "Non-nil when RUN still has agents (it is in `my-mas--runs')."
  (memq run my-mas--runs))

(defun my-mas--finished-p (run)
  "Non-nil when RUN reached a final status."
  (memq (my-mas-run-status run) '(done limit stalled aborted)))

(defun my-mas--run-for-root (root)
  "Return the active run of project ROOT, or nil."
  (seq-find (lambda (run) (equal (my-mas-run-root run) root)) my-mas--runs))

(defun my-mas--current-run ()
  "Return the run of this buffer or project; ask when ambiguous."
  (or (and (my-mas-run-p my-mas--run) (my-mas--active-p my-mas--run)
           my-mas--run)
      (let ((root (ignore-errors (my-dsh-acp--project-root 'no-fallback))))
        (and root (my-mas--run-for-root root)))
      (pcase my-mas--runs
        ('nil (user-error "MAS: no multi-agent run"))
        (`(,only) only)
        (runs
         (let* ((choices (mapcar (lambda (run)
                                   (cons (format "%s  %s"
                                                 (my-mas-run-id run)
                                                 (my-mas-run-root run))
                                         run))
                                 runs))
                (choice (completing-read "MAS run: " choices nil t)))
           (cdr (assoc choice choices)))))))

(defun my-mas--step-def (run &optional step)
  "Return the plist of STEP (default: RUN's current step)."
  (or (alist-get (or step (my-mas-run-step run))
                 (plist-get (my-mas--flow (my-mas-run-flow run)) :steps))
      (error "MAS: flow %s has no step %s"
             (my-mas-run-flow run) (or step (my-mas-run-step run)))))

(defun my-mas--agent (run role)
  "Return RUN's agent buffer for ROLE, or nil."
  (alist-get role (my-mas-run-agents run)))

(defun my-mas--role-harness (run role)
  "Return the harness playing ROLE in RUN."
  (alist-get role (my-mas-run-roles run)))

(defun my-mas--role-label (run role)
  "Return \"ROLE (Harness)\" for messages."
  (format "%s (%s)" role
          (my-dsh-acp--harness-name (my-mas--role-harness run role))))

(defun my-mas--elapsed (run)
  "Return RUN's running time as H:MM."
  (let ((secs (truncate (float-time
                         (time-subtract nil (my-mas-run-started run))))))
    (format "%d:%02d" (/ secs 3600) (/ (% secs 3600) 60))))

(defun my-mas--max (run)
  "Return the review-round limit of RUN's current phase.
The run's own limits (the C-c a m i menu) win; flows without phases
use `my-desktop-mas-max-iterations'."
  (or (alist-get (my-mas-run-phase run) (my-mas-run-limits run))
      my-desktop-mas-max-iterations))

(defun my-mas--rounds-left-p (run)
  "Non-nil when RUN's current phase may take another review round."
  (< (my-mas-run-n run) (my-mas--max run)))

;; ------------------------------------------------------------------
;; Notices and mode line
;; ------------------------------------------------------------------

(defun my-mas--notify (format-string &rest args)
  "Show a MAS notice (echo area, and a desktop notification if possible)."
  (let ((text (apply #'format format-string args)))
    (message "MAS: %s" text)
    (when (and (display-graphic-p) (require 'notifications nil t))
      (ignore-errors
        (notifications-notify :title "Emacs MAS" :body text)))))

(defun my-mas--state-label (run)
  "Return a short label of what RUN is doing."
  (let ((waiting (my-mas-run-waiting run)))
    (cond
     ((and (eq (my-mas-run-status run) 'running) waiting)
      (format "%s→%s" (my-mas-run-step run) (plist-get waiting :role)))
     ((eq (my-mas-run-status run) 'discuss)
      (format "%s→you" (my-mas-run-step run)))
     (t (symbol-name (my-mas-run-status run))))))

(defun my-mas--mode-line ()
  "Mode-line segment of the run of this agent buffer."
  (when (my-mas-run-p my-mas--run)
    (format " MAS[%s%s %d/%d %s]"
            (my-mas-run-flow my-mas--run)
            (if-let* ((phase (my-mas-run-phase my-mas--run)))
                (format ":%s" phase)
              "")
            (my-mas-run-n my-mas--run) (my-mas--max my-mas--run)
            (my-mas--state-label my-mas--run))))

(defun my-mas--refresh (run)
  "Redisplay the mode lines of RUN's agents."
  (dolist (entry (my-mas-run-agents run))
    (when (buffer-live-p (cdr entry))
      (with-current-buffer (cdr entry)
        (force-mode-line-update)))))

;; ------------------------------------------------------------------
;; Files: templates, run state, results
;; ------------------------------------------------------------------

(defun my-mas--file (run name)
  "Return the absolute path of NAME in RUN's folder."
  (expand-file-name name (my-mas-run-dir run)))

(defun my-mas--read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my-mas--render (template vars)
  "Replace {{key}} in TEMPLATE with values from VARS (string alist)."
  (replace-regexp-in-string
   "{{\\([a-z_]+\\)}}"
   (lambda (match)
     (let ((key (match-string 1 match)))
       (or (cdr (assoc key vars))
           (error "MAS: unknown template placeholder {{%s}}" key))))
   template t t))

(defun my-mas--template (flow name)
  "Return the prompt template NAME of FLOW (or a shared one)."
  (let ((file (expand-file-name (format "%s/%s.md" flow name)
                                my-desktop-mas-prompts-dir)))
    (unless (file-readable-p file)
      (user-error "MAS: missing prompt template %s" file))
    (my-mas--read-file file)))

(defun my-mas--vars (run def)
  "Return the template variables of step DEF in RUN."
  (let* ((n (my-mas-run-n run))
         (vars (append
                `(("run_id" . ,(my-mas-run-id run))
                  ("project" . ,(directory-file-name (my-mas-run-root run)))
                  ("run_dir" . ,(directory-file-name (my-mas-run-dir run)))
                  ("flow" . ,(symbol-name (my-mas-run-flow run)))
                  ("n" . ,(number-to-string n))
                  ("prev_n" . ,(number-to-string (1- n)))
                  ("max" . ,(number-to-string (my-mas--max run))))
                (my-mas-run-params run)))
         (output (when-let* ((output (plist-get def :output)))
                   (my-mas--render output vars)))
         (input (when-let* ((input (plist-get def :input)))
                  (my-mas--render input vars))))
    (setq vars (append `(("artifact" . ,(if output (my-mas--file run output) ""))
                         ("input" . ,(if input (my-mas--file run input) "")))
                       vars))
    (cons (cons "schema"
                (my-mas--render (my-mas--template "." "schema") vars))
          vars)))

(defun my-mas--json-read (file)
  "Parse FILE as a JSON object; return (ok . DATA) or (error . MESSAGE)."
  (cond
   ((not (file-exists-p file))
    (cons 'error (format "the result file %s was not written" file)))
   (t
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (cons 'ok (json-parse-buffer :object-type 'alist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object nil)))
      (error (cons 'error (format "%s is not valid JSON: %s"
                                  file (error-message-string err))))))))

(defun my-mas--findings (data)
  "Return the findings list of result DATA."
  (let ((findings (alist-get 'findings data)))
    (and (listp findings) findings)))

(defun my-mas--open-p (finding)
  "Non-nil when FINDING is still open (status open or missing)."
  (member (or (alist-get 'status finding) "open") '("open")))

(defun my-mas--open-findings (data)
  "Return the open findings of result DATA."
  (seq-filter #'my-mas--open-p (my-mas--findings data)))

(defun my-mas--validate (run def data)
  "Return a problem description for result DATA of step DEF, or nil."
  (let* ((findings (alist-get 'findings data))
         (verdict (alist-get 'verdict data))
         (allowed (cond ((plist-get def :verdicts))
                        ((plist-get def :critique) my-mas--critique-verdicts)
                        (t my-mas--work-verdicts))))
    (cond
     ((not (and (consp data) (consp (car data)) (symbolp (caar data))))
      "the result file must contain one JSON object")
     ((not (stringp (alist-get 'summary data)))
      "the field \"summary\" (a string) is missing")
     ((not (member verdict allowed))
      (format "\"verdict\" must be one of %s, got %S"
              (string-join allowed ", ") verdict))
     ((not (listp findings))
      "\"findings\" must be a JSON array")
     ((seq-find (lambda (f) (not (and (consp f)
                                      (stringp (alist-get 'id f)))))
                findings)
      "every finding must be an object with a string \"id\"")
     ((seq-find (lambda (field)
                  (let ((value (alist-get field data)))
                    (not (and (stringp value)
                              (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'"
                                              value)))))
                (plist-get def :capture))
      (format "the field(s) %s must be a kebab-case string"
              (mapconcat (lambda (f) (format "\"%s\"" f))
                         (plist-get def :capture) ", ")))
     ((and (plist-get def :approve-clean)
           (equal verdict "approve")
           (my-mas--open-findings data))
      (format "\"approve\" leaves open findings (%s); minor ones count \
too: mark each one resolved or conceded, or report changes_requested"
              (mapconcat (lambda (f) (alist-get 'id f))
                         (my-mas--open-findings data) ", ")))
     ((plist-get def :answers)
      (let* ((input (cdr (assoc "input" (my-mas--vars run def))))
             (asked (pcase (my-mas--json-read input)
                      (`(ok . ,in) (my-mas--open-findings in))))
             (answered (mapcar (lambda (f) (alist-get 'id f)) findings))
             (missing (seq-remove (lambda (id) (member id answered))
                                  (mapcar (lambda (f) (alist-get 'id f))
                                          asked))))
        (when missing
          (format "answer every open finding of %s; missing ids: %s"
                  input (string-join missing ", "))))))))

(defun my-mas--save (run)
  "Write RUN's state to run.json in its folder."
  (ignore-errors
    (let ((state
           `((id . ,(my-mas-run-id run))
             (flow . ,(symbol-name (my-mas-run-flow run)))
             (project . ,(my-mas-run-root run))
             (status . ,(symbol-name (my-mas-run-status run)))
             (step . ,(symbol-name (my-mas-run-step run)))
             (phase . ,(if (my-mas-run-phase run)
                           (symbol-name (my-mas-run-phase run))
                         ""))
             (iteration . ,(my-mas-run-n run))
             (max_iterations . ,(my-mas--max run))
             (reason . ,(or (my-mas-run-reason run) ""))
             (started . ,(format-time-string "%FT%T%z"
                                             (my-mas-run-started run)))
             (roles . ,(mapcar (lambda (r) (cons (car r) (symbol-name (cdr r))))
                               (my-mas-run-roles run)))
             (params . ,(mapcar (lambda (p) (cons (intern (car p)) (cdr p)))
                                (my-mas-run-params run)))
             (history
              . ,(vconcat
                  (mapcar (lambda (h)
                            `((step . ,(symbol-name (plist-get h :step)))
                              (role . ,(symbol-name (plist-get h :role)))
                              (iteration . ,(plist-get h :n))
                              (verdict . ,(plist-get h :verdict))
                              (file . ,(or (plist-get h :file) ""))
                              (time . ,(plist-get h :time))))
                          (reverse (my-mas-run-history run))))))))
      (with-temp-file (my-mas--file run "run.json")
        (insert (json-serialize state))))))

;; ------------------------------------------------------------------
;; Worktree check (reviewer must not edit the project)
;; ------------------------------------------------------------------

(defun my-mas--git (root &rest args)
  "Run git ARGS in ROOT; return its output, or nil on failure."
  (with-temp-buffer
    (let ((default-directory root))
      (when (eq 0 (apply #'call-process "git" nil t nil args))
        (buffer-string)))))

(defun my-mas--tamper-excludes (def vars)
  "Return the extra pathspecs step DEF may edit (its :may-edit), or nil."
  (when-let* ((path (plist-get def :may-edit)))
    (list (concat ":(exclude)" (my-mas--render path vars)))))

(defun my-mas--worktree-snapshot (root &optional excludes)
  "Return a snapshot of ROOT's worktree state, or nil outside git.
EXCLUDES are extra pathspecs to ignore."
  (when-let* (((file-exists-p (expand-file-name ".git" root)))
              (excludes (append my-mas--worktree-excludes excludes))
              (status (apply #'my-mas--git root "status" "--porcelain=v1"
                             "-uall" "--" "." excludes))
              (diff (apply #'my-mas--git root "diff" "--" "." excludes)))
    (list :status (split-string status "\n" t)
          :hash (secure-hash 'sha1 diff))))

(defun my-mas--worktree-changes (before after)
  "Describe the worktree changes between snapshots BEFORE and AFTER."
  (when (and before after
             (not (and (equal (plist-get before :status)
                              (plist-get after :status))
                       (equal (plist-get before :hash)
                              (plist-get after :hash)))))
    (let ((new (seq-difference (plist-get after :status)
                               (plist-get before :status))))
      (if new
          (string-join (seq-take new 10) "; ")
        "the content of already modified files"))))

(defun my-mas--git-exclude (root)
  "Ignore .mas/ in ROOT through .git/info/exclude (never a tracked file)."
  (let ((git (expand-file-name ".git" root)))
    (when (file-directory-p git)
      (let ((exclude (expand-file-name "info/exclude" git)))
        (unless (and (file-exists-p exclude)
                     (string-match-p "^/?\\.mas/?$"
                                     (my-mas--read-file exclude)))
          (make-directory (file-name-directory exclude) t)
          (write-region "\n# multi-agent runs (desktop-mas)\n/.mas/\n"
                        nil exclude t 'silent))))))

;; ------------------------------------------------------------------
;; Containers
;; ------------------------------------------------------------------

(defun my-mas--podman ()
  "Return the podman executable or nil."
  (executable-find my-desktop-podman-command))

(defun my-mas--launcher-args (run role)
  "Return the launcher options of ROLE's container in RUN."
  (let ((mount (or (alist-get role (my-mas-run-mounts run))
                   (alist-get role (plist-get (my-mas--flow (my-mas-run-flow run))
                                              :mounts))
                   "rw"))
        (id (my-mas-run-id run)))
    (append
     (list "--engine" "podman"
           "--role" (format "mas-%s" role)
           "--outbox" (directory-file-name (my-mas-run-dir run))
           "--label" (format "sem.mas-run=%s" id)
           "--label" (format "sem.mas-flow=%s" (my-mas-run-flow run))
           "--name" (format "mas-%s-%s" id role)
           "--timeout" (number-to-string my-desktop-mas-run-timeout))
     (unless (equal mount "rw")
       (list "--mount" mount)))))

(defun my-mas--container-ids-sync (id)
  "Return the IDs of all containers labelled with run ID (blocking)."
  (when-let* ((podman (my-mas--podman)))
    (with-temp-buffer
      (when (eq 0 (call-process podman nil t nil "ps" "-aq" "--filter"
                                (format "label=sem.mas-run=%s" id)))
        (split-string (buffer-string) "\n" t)))))

(defun my-mas--remove-containers-sync (id)
  "Remove every container of run ID now (blocking; for `kill-emacs-hook')."
  (when-let* ((podman (my-mas--podman))
              (ids (my-mas--container-ids-sync id)))
    (apply #'call-process podman nil nil nil "rm" "-f" ids)))

(defun my-mas--remove-containers (id)
  "Remove every container still labelled with run ID, asynchronously."
  (when-let* ((podman (my-mas--podman)))
    (let ((out (generate-new-buffer " *mas-podman-ps*")))
      (make-process
       :name "mas-podman-ps" :buffer out :noquery t
       :command (list podman "ps" "-aq" "--filter"
                      (format "label=sem.mas-run=%s" id))
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((ids (and (eq 0 (process-exit-status proc))
                           (with-current-buffer out
                             (split-string (buffer-string) "\n" t)))))
             (kill-buffer out)
             (when ids
               (make-process
                :name "mas-podman-rm" :buffer nil :noquery t
                :command (append (list podman "rm" "-f") ids)
                :sentinel
                (lambda (rm _event)
                  (when (memq (process-status rm) '(exit signal))
                    (message "MAS: run %s: %s %d leftover container(s)"
                             id (if (eq 0 (process-exit-status rm))
                                    "removed" "failed to remove")
                             (length ids)))))))))))))

(defvar my-mas--pending-sweeps nil
  "Run IDs whose agents were stopped but not yet swept by podman.")

(defun my-mas--sweep (id)
  "Remove the leftover containers of run ID and forget the sweep."
  (setq my-mas--pending-sweeps (delete id my-mas--pending-sweeps))
  (my-mas--remove-containers id))

(defun my-mas--kill-emacs ()
  "Remove the containers of all live runs before Emacs exits."
  (dolist (id (delete-dups (append (mapcar #'my-mas-run-id my-mas--runs)
                                   my-mas--pending-sweeps)))
    (ignore-errors (my-mas--remove-containers-sync id))))

(add-hook 'kill-emacs-hook #'my-mas--kill-emacs)

(defun my-mas--stop-agents (run)
  "Kill RUN's agent buffers (stopping their containers), then sweep."
  (setf (my-mas-run-stopping run) t)
  (when (timerp (my-mas-run-idle-timer run))
    (cancel-timer (my-mas-run-idle-timer run)))
  (setq my-mas--runs (delq run my-mas--runs))
  (push (my-mas-run-id run) my-mas--pending-sweeps)
  (dolist (entry (my-mas-run-agents run))
    (let ((buffer (cdr entry)))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))))
  (run-at-time my-mas--remove-delay nil #'my-mas--sweep (my-mas-run-id run)))

;; ------------------------------------------------------------------
;; Report
;; ------------------------------------------------------------------

(defun my-mas--cell (text)
  "Return TEXT as a one-line org table cell."
  (replace-regexp-in-string "[|\n\r]+" " " (or text "")))

(defun my-mas--phase-rounds (run phase)
  "Return the review rounds RUN took in PHASE (0 if it never got there)."
  (let ((rounds 0))
    (dolist (h (my-mas-run-history run) rounds)
      (when (and (eq (plist-get h :phase) phase)
                 (plist-get (my-mas--step-def run (plist-get h :step))
                            :critique))
        (setq rounds (max rounds (plist-get h :n)))))))

(defun my-mas--write-report (run)
  "Write report.org for RUN; return its path."
  (let ((file (my-mas--file run "report.org"))
        (flow (my-mas-run-flow run))
        (critique (my-mas-run-last-critique run)))
    (with-temp-file file
      (insert (format "#+title: MAS %s run %s\n\n" flow (my-mas-run-id run)))
      (insert (format "- Status :: %s\n" (my-mas-run-status run)))
      (when (my-mas-run-reason run)
        (insert (format "- Reason :: %s\n" (my-mas-run-reason run))))
      (insert (format "- Project :: %s\n" (my-mas-run-root run)))
      (insert (format "- Roles :: %s\n"
                      (mapconcat (lambda (r) (my-mas--role-label run (car r)))
                                 (my-mas-run-roles run) ", ")))
      (dolist (param (my-mas-run-params run))
        (insert (format "- %s :: %s\n" (car param) (cdr param))))
      (if (my-mas-run-limits run)
          (insert (format "- Rounds :: %s\n"
                          (mapconcat
                           (lambda (limit)
                             (format "%s %d / %d" (car limit)
                                     (my-mas--phase-rounds run (car limit))
                                     (cdr limit)))
                           (my-mas-run-limits run) ", ")))
        (insert (format "- Iterations :: %d / %d\n" (my-mas-run-n run)
                        (my-mas--max run))))
      (insert (format "- Duration :: %s\n" (my-mas--elapsed run)))
      (insert "\n* Steps\n\n| # | Step | Agent | Verdict | Summary | File |\n")
      (insert "|---+------+-------+---------+---------+------|\n")
      (let ((i 0))
        (dolist (h (reverse (my-mas-run-history run)))
          (insert (format "| %d | %s | %s | %s | %s | %s |\n"
                          (cl-incf i) (plist-get h :step)
                          (my-mas--role-label run (plist-get h :role))
                          (plist-get h :verdict)
                          (my-mas--cell (plist-get h :summary))
                          (if-let* ((file (plist-get h :file)))
                              (format "[[file:%s][%s]]" file
                                      (file-name-nondirectory file))
                            "")))))
      (let ((open (and critique (my-mas--open-findings critique))))
        (insert (format "\n* Open findings (%d)\n\n" (length open)))
        (dolist (f open)
          (insert (format "- [%s] (%s) %s :: %s\n"
                          (alist-get 'id f)
                          (or (alist-get 'severity f) "?")
                          (my-mas--cell (alist-get 'title f))
                          (my-mas--cell (or (alist-get 'reason f)
                                            (alist-get 'detail f)))))))
      (insert "\n* Next\n\n")
      (pcase flow
        ('openspec
         (insert "Nothing was committed.  Review the changes and commit \
them yourself (C-c g g).\n"))
        ('architecture
         (insert (format "Design file: [[file:%s]]\n"
                         (cdr (assoc "design_file"
                                     (my-mas-run-params run))))))
        ('pair
         (insert (format "Brief: [[file:%s]], plan: [[file:%s]]\n\n"
                         (my-mas--file run "brief.md")
                         (my-mas--file run "plan.md")))
         (insert "Nothing was committed.  Review the diff (C-c g) and \
commit it yourself.\n")))
      (insert "\nTranscripts: .agent-shell/transcripts/ in the project.\n"))
    file))

;; ------------------------------------------------------------------
;; Engine
;; ------------------------------------------------------------------

(defun my-mas--pause (run reason resume)
  "Pause RUN for REASON; RESUME is called by `my-mas-continue'."
  (when (timerp (my-mas-run-idle-timer run))
    (cancel-timer (my-mas-run-idle-timer run)))
  (setf (my-mas-run-status run) 'paused
        (my-mas-run-reason run) reason
        (my-mas-run-pending run) resume
        (my-mas-run-waiting run) nil
        (my-mas-run-pause-requested run) nil)
  (my-mas--save run)
  (my-mas--refresh run)
  (my-mas--notify "%s paused: %s (C-c a m c continues, k aborts)"
                  (my-mas-run-id run) reason))

(defun my-mas--finish (run status &optional reason force-stop)
  "Finish RUN with STATUS and REASON; write the report.
Stop the agents when `my-desktop-mas-stop-agents-on-finish' or
FORCE-STOP is non-nil."
  (when (timerp (my-mas-run-idle-timer run))
    (cancel-timer (my-mas-run-idle-timer run)))
  (setf (my-mas-run-status run) status
        (my-mas-run-reason run) (or reason (my-mas-run-reason run))
        (my-mas-run-waiting run) nil
        (my-mas-run-pending run) nil)
  (my-mas--save run)
  (let ((report (ignore-errors (my-mas--write-report run))))
    (my-mas--refresh run)
    (when (or force-stop my-desktop-mas-stop-agents-on-finish)
      (my-mas--stop-agents run))
    (when report
      (display-buffer (find-file-noselect report)))
    (my-mas--notify "%s finished: %s%s" (my-mas-run-id run) status
                    (if reason (format " (%s)" reason) ""))))

(defun my-mas--run-step (run step)
  "Enter STEP of RUN."
  (let ((def (my-mas--step-def run step)))
    (setf (my-mas-run-step run) step
          (my-mas-run-reason run) nil)
    (when-let* ((phase (plist-get def :phase))
                ((not (eq phase (my-mas-run-phase run)))))
      (my-mas--enter-phase run phase))
    (cond
     ((plist-get def :report)
      (my-mas--finish run 'done))
     ((plist-get def :discuss)
      (my-mas--discuss run step))
     ((plist-get def :gate)
      (if (memq (plist-get def :gate) my-desktop-mas-gates)
          (progn
            (setf (my-mas-run-status run) 'gate
                  (my-mas-run-pending run)
                  (lambda () (my-mas--run-step run (plist-get def :next))))
            (my-mas--save run)
            (my-mas--refresh run)
            (my-mas--notify "%s waits at gate %s: check the work, then \
C-c a m c" (my-mas-run-id run) (plist-get def :gate)))
        (my-mas--run-step run (plist-get def :next))))
     ((and (plist-get def :bump) (not (my-mas--rounds-left-p run)))
      (my-mas--finish run 'limit
                      (format "%d %sreview rounds without agreement"
                              (my-mas-run-n run)
                              (if (my-mas-run-phase run)
                                  (format "%s " (my-mas-run-phase run))
                                ""))))
     (t
      (when (plist-get def :bump)
        (cl-incf (my-mas-run-n run)))
      (my-mas--send run step)))))

(defun my-mas--enter-phase (run phase)
  "Start PHASE of RUN: a fresh round count, stall check and critique."
  (setf (my-mas-run-phase run) phase
        (my-mas-run-n run) 1
        (my-mas-run-stall run) 0
        (my-mas-run-open-prev run) nil
        (my-mas-run-last-critique run) nil))

(defun my-mas--discuss (run step)
  "Send discussion STEP's prompt in RUN, then leave the agent to the operator.
No result file is expected and turns are not watched (the status is
`discuss'); `my-mas-continue' ends the step."
  (let* ((def (my-mas--step-def run step))
         (role (plist-get def :role))
         (buffer (my-mas--agent run role))
         (vars (my-mas--vars run def)))
    (if (not (buffer-live-p buffer))
        (my-mas--finish run 'aborted (format "the %s agent is gone" role) t)
      (when (plist-get def :tamper)
        (setf (my-mas-run-snapshot run)
              (my-mas--worktree-snapshot
               (my-mas-run-root run) (my-mas--tamper-excludes def vars))))
      (setf (my-mas-run-status run) 'discuss
            (my-mas-run-waiting run) nil
            (my-mas-run-pending run) nil)
      (my-mas--save run)
      (my-mas--refresh run)
      (condition-case err
          (progn
            (with-current-buffer buffer
              (agent-shell-insert
               :text (my-mas--render (my-mas--template
                                      (my-mas-run-flow run)
                                      (plist-get def :prompt))
                                     vars)
               :submit t :no-focus t :shell-buffer buffer))
            (when-let* ((window (get-buffer-window buffer)))
              (select-window window))
            (my-mas--notify "%s: talk to the %s; C-c a m c when you are \
done" (my-mas-run-id run) (my-mas--role-label run role)))
        (error
         (my-mas--pause run (format "could not send %s to %s: %s" step
                                    (my-mas--role-label run role)
                                    (error-message-string err))
                        (lambda () (my-mas--discuss run step))))))))

(defun my-mas--read-choice (run def)
  "Ask the operator for the next step after discussion step DEF of RUN.
Return :next when DEF has no :choices.  A choice marked `round' is
offered only while the phase has review rounds left."
  (if-let* ((choices (plist-get def :choices)))
      (let* ((offered (seq-filter (lambda (c)
                                    (or (not (eq (nth 4 c) 'round))
                                        (my-mas--rounds-left-p run)))
                                  choices))
             (answer (read-multiple-choice
                      (format "MAS %s%s" (my-mas-run-step run)
                              (if (= (length offered) (length choices))
                                  ""
                                (format " (%s rounds used up)"
                                        (my-mas-run-phase run))))
                      (mapcar (lambda (c) (list (nth 0 c) (nth 1 c) (nth 3 c)))
                              offered))))
        (cons (nth 2 (assq (car answer) offered)) (nth 1 answer)))
    (cons (plist-get def :next) "done")))

(defun my-mas--end-discussion (run)
  "End RUN's discussion step: check the worktree, then go on.
Quitting the choice prompt keeps the discussion open."
  (let* ((step (my-mas-run-step run))
         (def (my-mas--step-def run step))
         (role (plist-get def :role))
         (choice (my-mas--read-choice run def))
         (changes (and (plist-get def :tamper)
                       (my-mas--worktree-changes
                        (my-mas-run-snapshot run)
                        (my-mas--worktree-snapshot
                         (my-mas-run-root run)
                         (my-mas--tamper-excludes
                          def (my-mas--vars run def)))))))
    (when (and changes
               (not (y-or-n-p (format "%s changed project files during %s: \
%s.  Go on anyway? " (my-mas--role-label run role) step changes))))
      (user-error "MAS: still in %s" step))
    (push (list :step step :role role :n (my-mas-run-n run)
                :phase (my-mas-run-phase run) :verdict (cdr choice)
                :summary "operator" :time (format-time-string "%FT%T%z"))
          (my-mas-run-history run))
    (setf (my-mas-run-status run) 'running)
    (if (eq (car choice) 'abort)
        (my-mas--finish run 'aborted "aborted by the operator" t)
      (my-mas--run-step run (car choice)))))

(defun my-mas--start-idle-timer (run)
  "Start the repeating still-working notice of RUN's current step."
  (when (timerp (my-mas-run-idle-timer run))
    (cancel-timer (my-mas-run-idle-timer run)))
  (let ((started (current-time)))
    (setf (my-mas-run-idle-timer run)
          (run-at-time my-desktop-mas-turn-idle-timeout
                       my-desktop-mas-turn-idle-timeout
                       (lambda ()
                         (when-let* ((waiting (my-mas-run-waiting run)))
                           (my-mas--notify
                            "%s: %s still on %s after %d min"
                            (my-mas-run-id run)
                            (my-mas--role-label run (plist-get waiting :role))
                            (my-mas-run-step run)
                            (/ (truncate (float-time
                                          (time-subtract nil started)))
                               60))))))))

(defun my-mas--send (run step &optional reminder)
  "Send STEP's prompt (or the REMINDER text) to its agent in RUN."
  (let* ((def (my-mas--step-def run step))
         (role (plist-get def :role))
         (buffer (my-mas--agent run role))
         (vars (my-mas--vars run def))
         (output (cdr (assoc "artifact" vars))))
    (if (not (buffer-live-p buffer))
        (my-mas--finish run 'aborted
                        (format "the %s agent is gone" role) t)
      (unless reminder
        ;; A retried step must not validate a stale result.
        (when (file-exists-p output)
          (delete-file output))
        (when (plist-get def :tamper)
          (setf (my-mas-run-snapshot run)
                (my-mas--worktree-snapshot
                 (my-mas-run-root run) (my-mas--tamper-excludes def vars)))))
      (setf (my-mas-run-status run) 'running
            (my-mas-run-waiting run)
            (list :step step :role role :output output
                  :reminded (and reminder t)))
      (my-mas--save run)
      (my-mas--refresh run)
      (condition-case err
          (with-current-buffer buffer
            (agent-shell-insert
             :text (or reminder
                       (my-mas--render (my-mas--template
                                        (my-mas-run-flow run)
                                        (plist-get def :prompt))
                                       vars))
             :submit t :no-focus t :shell-buffer buffer)
            (my-mas--start-idle-timer run))
        (error
         (my-mas--pause run (format "could not send %s to %s: %s" step
                                    (my-mas--role-label run role)
                                    (error-message-string err))
                        (lambda () (my-mas--send run step))))))))

(defun my-mas--on-turn-complete (run role event)
  "Handle the end of ROLE's turn in RUN (agent-shell EVENT)."
  (let ((waiting (my-mas-run-waiting run)))
    (when (and (my-mas--active-p run)
               (eq (my-mas-run-status run) 'running)
               waiting
               (eq (plist-get waiting :role) role))
      (when (timerp (my-mas-run-idle-timer run))
        (cancel-timer (my-mas-run-idle-timer run)))
      (let ((stop (alist-get :stop-reason (alist-get :data event)))
            (step (plist-get waiting :step)))
        (if (equal stop "end_turn")
            ;; Leave agent-shell's own turn bookkeeping first.
            (run-at-time 0.5 nil #'my-mas--complete-step run)
          (my-mas--pause run (format "%s stopped %s: %s"
                                     (my-mas--role-label run role) step
                                     (or stop "no stop reason"))
                         (lambda () (my-mas--send run step))))))))

(defun my-mas--complete-step (run)
  "Check the result of RUN's current step and move on."
  (when-let* ((waiting (my-mas-run-waiting run))
              ((my-mas--active-p run)))
    (let* ((step (plist-get waiting :step))
           (role (plist-get waiting :role))
           (def (my-mas--step-def run step))
           (output (plist-get waiting :output))
           (read (my-mas--json-read output))
           (problem (if (eq (car read) 'ok)
                        (my-mas--validate run def (cdr read))
                      (cdr read))))
      (cond
       ((and problem (not (plist-get waiting :reminded)))
        (my-mas--send
         run step
         (format "The orchestrator could not accept your result: %s.
Fix it now: write the result file %s as described in the step \
instructions (one JSON object with verdict, summary and findings).  \
Do not redo the work itself."
                 problem output)))
       (problem
        (my-mas--pause run (format "%s result rejected: %s" step problem)
                       (lambda () (my-mas--send run step))))
       (t
        (let ((data (cdr read)))
          (setf (my-mas-run-waiting run) nil)
          (dolist (field (plist-get def :capture))
            (setf (my-mas-run-params run)
                  (cons (cons (symbol-name field) (alist-get field data))
                        (assoc-delete-all (symbol-name field)
                                          (my-mas-run-params run)))))
          (push (list :step step :role role :n (my-mas-run-n run)
                      :phase (my-mas-run-phase run)
                      :verdict (alist-get 'verdict data)
                      :summary (alist-get 'summary data)
                      :file output
                      :time (format-time-string "%FT%T%z"))
                (my-mas-run-history run))
          (let ((changes (and (plist-get def :tamper)
                              (my-mas--worktree-changes
                               (my-mas-run-snapshot run)
                               (my-mas--worktree-snapshot
                                (my-mas-run-root run)
                                (my-mas--tamper-excludes
                                 def (my-mas--vars run def)))))))
            (if changes
                (my-mas--pause
                 run (format "%s changed project files during %s: %s \
\(check them; continue keeps going)" (my-mas--role-label run role) step changes)
                 (lambda () (my-mas--advance run def data)))
              (my-mas--advance run def data)))))))))

(defun my-mas--stalled-p (run data)
  "Record critique DATA in RUN; non-nil when the run is stalled."
  (let ((open (sort (mapcar (lambda (f)
                              (format "%s %s" (alist-get 'id f)
                                      (or (alist-get 'title f) "")))
                            (my-mas--open-findings data))
                    #'string<)))
    (if (and open (equal open (my-mas-run-open-prev run)))
        (cl-incf (my-mas-run-stall run))
      (setf (my-mas-run-stall run) 0))
    (setf (my-mas-run-open-prev run) open)
    (and my-desktop-mas-stall-rounds
         (>= (my-mas-run-stall run) my-desktop-mas-stall-rounds))))

(defun my-mas--advance (run def data)
  "Leave step DEF of RUN with result DATA."
  (let* ((verdict (alist-get 'verdict data))
         (step (my-mas-run-step run))
         (next (if (plist-get def :critique)
                   (alist-get (intern verdict) (plist-get def :on))
                 (plist-get def :next))))
    (when (plist-get def :critique)
      (setf (my-mas-run-last-critique run) data))
    (cond
     ((equal verdict "blocked")
      (my-mas--pause run (format "%s reports blocked: %s" step
                                 (alist-get 'summary data))
                     (lambda () (my-mas--send run step))))
     ((and (plist-get def :critique)
           (equal verdict "changes_requested")
           (my-mas--stalled-p run data))
      (my-mas--finish run 'stalled
                      (format "the same open findings %d rounds in a row"
                              (1+ (my-mas-run-stall run)))))
     ((null next)
      (my-mas--finish run 'aborted
                      (format "no transition from %s on %s" step verdict) t))
     ((my-mas-run-pause-requested run)
      (my-mas--pause run "paused on request"
                     (lambda () (my-mas--run-step run next))))
     (t (my-mas--run-step run next)))))

;; ------------------------------------------------------------------
;; Starting a run
;; ------------------------------------------------------------------

(defun my-mas--layout (run)
  "Show RUN's agents side by side in the selected frame."
  (let ((buffers (seq-filter #'buffer-live-p
                             (mapcar #'cdr (my-mas-run-agents run)))))
    (when (fboundp 'persp-add-buffer)
      (mapc #'persp-add-buffer buffers))
    (when buffers
      (ignore-errors (delete-other-windows))
      (switch-to-buffer (car buffers))
      (when (cdr buffers)
        (set-window-buffer (split-window-right) (cadr buffers))))))

(defun my-mas--setup-agent (run role buffer)
  "Tie agent BUFFER (ROLE) to RUN: mode line and event handlers."
  (with-current-buffer buffer
    (setq-local my-mas--run run)
    (setq-local mode-line-misc-info
                (cons '(:eval (my-mas--mode-line)) mode-line-misc-info)))
  (agent-shell-subscribe-to
   :shell-buffer buffer :event 'turn-complete
   :on-event (lambda (event) (my-mas--on-turn-complete run role event)))
  (agent-shell-subscribe-to
   :shell-buffer buffer :event 'error
   :on-event (lambda (event)
               (my-mas--notify "%s: ACP error from %s: %s"
                               (my-mas-run-id run)
                               (my-mas--role-label run role)
                               (alist-get :message
                                          (alist-get :data event)))))
  (agent-shell-subscribe-to
   :shell-buffer buffer :event 'clean-up
   :on-event (lambda (_event)
               (unless (my-mas-run-stopping run)
                 ;; Outside the kill hook: stopping kills the other agent.
                 (run-at-time
                  0 nil
                  (lambda ()
                    (cond
                     ((my-mas-run-stopping run))
                     ((my-mas--finished-p run) (my-mas--stop-agents run))
                     (t (my-mas--finish run 'aborted
                                        (format "the %s agent buffer was \
killed" role)
                                        t)))))))))

(defun my-mas--start (flow params &optional roles settings)
  "Start a FLOW run in the current project with PARAMS (string alist).
ROLES maps each role to a harness, as (ROLE . HARNESS); it defaults to
FLOW's entry in `my-desktop-mas-roles'.  SETTINGS is a plist of
per-run overrides: :limits (PHASE . ROUNDS), :mounts (ROLE . MOUNT)
and :options (ROLE . ACP-CONFIG-OPTIONS)."
  (my-dsh-acp--require-agent-shell)
  (unless (my-mas--podman)
    (user-error "MAS: `%s' not found; the agents need podman"
                my-desktop-podman-command))
  (let* ((root (my-dsh-acp--project-root))
         (roles (my-mas--check-roles
                 flow (or roles (my-mas--flow-roles flow))))
         (now (current-time))
         ;; The run id names containers and labels; the folder name
         ;; follows `my-desktop-mas-run-dir-time-format'.
         (id (format "%s-%s" (format-time-string "%Y%m%d-%H%M%S" now) flow))
         (run (my-mas--make-run
               :id id :flow flow :roles roles :root root
               :dir (file-name-as-directory
                     (expand-file-name
                      (format ".mas/%s-%s"
                              (format-time-string
                               my-desktop-mas-run-dir-time-format now)
                              flow)
                      root))
               :params params :status 'starting
               :step (plist-get (my-mas--flow flow) :start)
               :limits (plist-get settings :limits)
               :mounts (plist-get settings :mounts)
               :options (plist-get settings :options)
               :started now)))
    (when-let* ((other (my-mas--run-for-root root)))
      (user-error "MAS: run %s is still active in this project \
\(C-c a m k aborts it)" (my-mas-run-id other)))
    (make-directory (my-mas-run-dir run) t)
    (my-mas--git-exclude root)
    (push run my-mas--runs)
    (condition-case err
        (dolist (entry (my-mas--flow-roles flow))
          (let* ((role (car entry))
                 (harness (alist-get role roles))
                 (buffer (my-dsh-acp-start-owned
                          :root root :harness harness :owner id
                          :extra-args (my-mas--launcher-args run role)
                          :config-options (alist-get role
                                                     (my-mas-run-options run))
                          :buffer-name (format "%s MAS %s"
                                               (my-dsh-acp--harness-name
                                                harness)
                                               role))))
            (unless (buffer-live-p buffer)
              (error "agent-shell did not start the %s agent" role))
            (setf (my-mas-run-agents run)
                  (append (my-mas-run-agents run) (list (cons role buffer))))
            (my-mas--setup-agent run role buffer)))
      (error
       (my-mas--finish run 'aborted
                       (format "starting the agents failed: %s"
                               (error-message-string err))
                       t)
       (signal (car err) (cdr err))))
    (my-mas--layout run)
    (my-mas--notify "%s started (%s); max %s rounds" id
                    (plist-get (my-mas--flow flow) :title)
                    (if-let* ((limits (my-mas-run-limits run)))
                        (mapconcat (lambda (l) (format "%d %s" (cdr l) (car l)))
                                   limits ", ")
                      my-desktop-mas-max-iterations))
    (my-mas--run-step run (my-mas-run-step run))
    run))

;; ------------------------------------------------------------------
;; Commands
;; ------------------------------------------------------------------

(defun my-mas--slug (text)
  "Return a short kebab-case slug of TEXT."
  (let ((slug (string-trim (replace-regexp-in-string
                            "[^a-z0-9]+" "-" (downcase text))
                           "-+" "-+")))
    (if (string-empty-p slug) "design"
      (string-trim-right (substring slug 0 (min 48 (length slug))) "-+"))))

;;;###autoload
(defun my-mas-openspec (request &optional roles)
  "Run the openspec flow for REQUEST in this project.
REQUEST names an existing change in openspec/changes/ or describes a
new one.  The lead drafts the change, the implementer implements it,
the lead verifies and reviews until it approves, then archives it.
Nothing is committed.  Interactively, ask which harness plays each
role; ROLES is (ROLE . HARNESS), default `my-desktop-mas-roles'."
  (interactive
   (list (read-string "openspec change (existing name, or describe a new one): ")
         (my-mas--read-roles 'openspec)))
  (when (string-empty-p (string-trim request))
    (user-error "MAS: the change request is empty"))
  (my-mas--start 'openspec `(("request" . ,(string-trim request))) roles))

;;;###autoload
(defun my-mas-architecture (topic design-file &optional roles)
  "Run the architecture flow on TOPIC, writing the design to DESIGN-FILE.
The architect writes the design, the critic roasts it against the
current worktree, the architect fixes it or pushes back, until the
critic approves.  DESIGN-FILE is relative to the project root.
Interactively, ask which harness plays each role; ROLES is
\(ROLE . HARNESS), default `my-desktop-mas-roles'."
  (interactive
   (let* ((topic (read-string "Architecture topic: "))
          (default (format "docs/design/%s.md" (my-mas--slug topic))))
     (list topic
           (read-string (format-prompt "Design file (in the project)"
                                       default)
                        nil nil default)
           (my-mas--read-roles 'architecture))))
  (when (string-empty-p (string-trim topic))
    (user-error "MAS: the topic is empty"))
  (let* ((root (my-dsh-acp--project-root))
         (file (expand-file-name design-file root)))
    (unless (and (file-in-directory-p file root)
                 (not (file-in-directory-p file (expand-file-name ".mas" root)))
                 (not (file-in-directory-p file (expand-file-name ".git" root))))
      (user-error "MAS: the design file must be inside the project: %s" file))
    (my-mas--start 'architecture `(("topic" . ,(string-trim topic))
                                   ("design_file" . ,file))
                   roles)))

(defconst my-mas--no-task
  "(none yet: ask the operator what to build)"
  "Task text of a pair run started without one.")

(defun my-mas--model-options (models)
  "Return per-role ACP config options for MODELS, as (ROLE . MODEL).
Roles without a model (nil or empty) keep their harness default."
  (delq nil (mapcar (lambda (entry)
                      (when (and (stringp (cdr entry))
                                 (not (string-empty-p (cdr entry))))
                        (list (car entry) (cons "model" (cdr entry)))))
                    models)))

;;;###autoload
(defun my-mas-pair (task &optional roles settings)
  "Run the implementer + reviewer flow on TASK in this project.
You discuss TASK with the implementer first; `my-mas-continue' starts
the plan, which the reviewer roasts until it approves.  The
implementer then presents the plan; once you approve it, it writes the
code and the reviewer reviews it until it approves.  Nothing is
committed.  TASK may be empty (the implementer asks).  ROLES is
\(ROLE . HARNESS), default `my-desktop-mas-roles'; SETTINGS overrides
the per-run settings (see `my-mas--start'), defaulting to
`my-desktop-mas-pair-rounds', `my-desktop-mas-pair-reviewer-mount' and
`my-desktop-mas-pair-models'.  Interactively, use the menu
`my-mas-pair-menu' (C-c a m i)."
  (interactive (list (read-string "Task (empty: discuss it first): ")))
  (let ((mount (or (plist-get settings :mounts)
                   `((reviewer . ,my-desktop-mas-pair-reviewer-mount)))))
    (unless (member (alist-get 'reviewer mount) '("overlay" "ro"))
      (user-error "MAS: the reviewer mount must be overlay or ro, not %S"
                  (alist-get 'reviewer mount)))
    (my-mas--start
     'pair `(("task" . ,(if (string-empty-p (string-trim task))
                            my-mas--no-task
                          (string-trim task))))
     roles
     (list :limits (or (plist-get settings :limits)
                       (copy-alist my-desktop-mas-pair-rounds))
           :mounts mount
           :options (or (plist-get settings :options)
                        (my-mas--model-options my-desktop-mas-pair-models))))))

;; The C-c a m i menu: settings of the next pair run, gptel-menu style.

(defvar my-mas--pair-settings nil
  "Settings of the next pair run, edited in `my-mas-pair-menu'.
A plist (:task :roles :models :limits :mount); nil until the menu
first opens, then kept for the session (D resets it).")

(defun my-mas--pair-defaults ()
  "Return the pair-run settings from the user configuration."
  (list :task ""
        :roles (copy-alist (my-mas--flow-roles 'pair))
        :models (copy-alist my-desktop-mas-pair-models)
        :limits (copy-alist my-desktop-mas-pair-rounds)
        :mount my-desktop-mas-pair-reviewer-mount))

(defun my-mas--pair-get (key)
  "Return setting KEY of the next pair run."
  (unless my-mas--pair-settings
    (setq my-mas--pair-settings (my-mas--pair-defaults)))
  (plist-get my-mas--pair-settings key))

(defun my-mas--pair-put (key value)
  "Set setting KEY of the next pair run to VALUE."
  (my-mas--pair-get key)
  (setq my-mas--pair-settings (plist-put my-mas--pair-settings key value)))

(defun my-mas--pair-harness-default-model (harness)
  "Return HARNESS's default model option, or nil."
  (cdr (assoc "model" (symbol-value
                       (my-dsh-acp--harness-get harness :options-var)))))

(defun my-mas--pair-value (text)
  "Return TEXT in the transient value face."
  (propertize text 'face 'transient-value))

(defun my-mas--pair-describe-harness (role)
  "Menu label of ROLE's harness."
  (format "%-12s %s" (capitalize (symbol-name role))
          (my-mas--pair-value
           (my-dsh-acp--harness-name
            (alist-get role (my-mas--pair-get :roles))))))

(defun my-mas--pair-describe-model (role)
  "Menu label of ROLE's model."
  (let ((model (alist-get role (my-mas--pair-get :models))))
    (format "%-12s %s" "  model"
            (if (and model (not (string-empty-p model)))
                (my-mas--pair-value model)
              (propertize
               (format "%s (harness default)"
                       (or (my-mas--pair-harness-default-model
                            (alist-get role (my-mas--pair-get :roles)))
                           "agent's own"))
               'face 'transient-inactive-value)))))

(defun my-mas--pair-read-harness (role)
  "Ask for ROLE's harness; a new harness drops ROLE's model."
  (let* ((choices (mapcar (lambda (entry)
                            (cons (plist-get (cdr entry) :name) (car entry)))
                          my-dsh-acp--harnesses))
         (completion-extra-properties '(:display-sort-function identity))
         (current (alist-get role (my-mas--pair-get :roles)))
         (harness (cdr (assoc (completing-read
                               (format-prompt "%s" (my-dsh-acp--harness-name
                                                    current)
                                              (capitalize (symbol-name role)))
                               choices nil t nil nil
                               (my-dsh-acp--harness-name current))
                              choices)))
         (roles (copy-alist (my-mas--pair-get :roles))))
    (unless (eq harness current)
      (setf (alist-get role roles) harness)
      (my-mas--pair-put :roles roles)
      (my-mas--pair-put :models (assq-delete-all
                                 role (copy-alist (my-mas--pair-get :models)))))))

(defun my-mas--pair-read-model (role)
  "Ask for ROLE's model; empty input means the harness default."
  (let* ((harness (alist-get role (my-mas--pair-get :roles)))
         (models (copy-alist (my-mas--pair-get :models)))
         (choices (delete-dups
                   (delq nil (cons (my-mas--pair-harness-default-model harness)
                                   (alist-get harness
                                              my-desktop-mas-model-choices)))))
         (model (string-trim
                 (completing-read
                  (format "%s model (%s; empty: harness default): "
                          (capitalize (symbol-name role))
                          (my-dsh-acp--harness-name harness))
                  choices nil nil (alist-get role models)))))
    (setf (alist-get role models nil t) (and (not (string-empty-p model))
                                             model))
    (my-mas--pair-put :models models)))

(defun my-mas--pair-read-rounds (phase)
  "Ask for the review-round limit of PHASE."
  (let ((limits (copy-alist (my-mas--pair-get :limits)))
        (rounds (read-number (format "%s review rounds: "
                                     (capitalize (symbol-name phase)))
                             (alist-get phase (my-mas--pair-get :limits)))))
    (unless (and (integerp rounds) (> rounds 0))
      (user-error "MAS: rounds must be a positive integer"))
    (setf (alist-get phase limits) rounds)
    (my-mas--pair-put :limits limits)))

(defun my-mas-pair-set-task ()
  "Set the task of the next pair run (empty: discuss it first)."
  (interactive)
  (my-mas--pair-put :task (read-string "Task (empty: discuss it first): "
                                       (my-mas--pair-get :task))))

(defun my-mas-pair-set-implementer ()
  "Set the implementer's harness of the next pair run."
  (interactive)
  (my-mas--pair-read-harness 'implementer))

(defun my-mas-pair-set-implementer-model ()
  "Set the implementer's model of the next pair run."
  (interactive)
  (my-mas--pair-read-model 'implementer))

(defun my-mas-pair-set-reviewer ()
  "Set the reviewer's harness of the next pair run."
  (interactive)
  (my-mas--pair-read-harness 'reviewer))

(defun my-mas-pair-set-reviewer-model ()
  "Set the reviewer's model of the next pair run."
  (interactive)
  (my-mas--pair-read-model 'reviewer))

(defun my-mas-pair-toggle-mount ()
  "Switch the reviewer's project mount between overlay and ro."
  (interactive)
  (my-mas--pair-put :mount (if (equal (my-mas--pair-get :mount) "overlay")
                               "ro"
                             "overlay")))

(defun my-mas-pair-set-plan-rounds ()
  "Set the plan review-round limit of the next pair run."
  (interactive)
  (my-mas--pair-read-rounds 'plan))

(defun my-mas-pair-set-code-rounds ()
  "Set the code review-round limit of the next pair run."
  (interactive)
  (my-mas--pair-read-rounds 'code))

(defun my-mas-pair-reset ()
  "Reset the next pair run's settings to the configured defaults."
  (interactive)
  (setq my-mas--pair-settings (my-mas--pair-defaults)))

(defun my-mas-pair-start ()
  "Start a pair run with the settings of `my-mas-pair-menu'."
  (interactive)
  (my-mas-pair (my-mas--pair-get :task)
               (copy-alist (my-mas--pair-get :roles))
               (list :limits (copy-alist (my-mas--pair-get :limits))
                     :mounts `((reviewer . ,(my-mas--pair-get :mount)))
                     :options (my-mas--model-options
                               (my-mas--pair-get :models)))))

(transient-define-prefix my-mas-pair-menu ()
  "Implementer + reviewer run: check the settings, C-c RET starts."
  [["Task"
    ("-t" my-mas-pair-set-task :transient t
     :description
     (lambda ()
       (format "%-12s %s" "Task"
               (let ((task (my-mas--pair-get :task)))
                 (if (string-empty-p (string-trim task))
                     (propertize "(discuss it first)"
                                 'face 'transient-inactive-value)
                   (my-mas--pair-value (truncate-string-to-width
                                        task 50 nil nil "…")))))))]]
  [["Implementer (left)"
    ("-i" my-mas-pair-set-implementer :transient t
     :description (lambda () (my-mas--pair-describe-harness 'implementer)))
    ("-I" my-mas-pair-set-implementer-model :transient t
     :description (lambda () (my-mas--pair-describe-model 'implementer)))]
   ["Reviewer (right)"
    ("-r" my-mas-pair-set-reviewer :transient t
     :description (lambda () (my-mas--pair-describe-harness 'reviewer)))
    ("-R" my-mas-pair-set-reviewer-model :transient t
     :description (lambda () (my-mas--pair-describe-model 'reviewer)))
    ("-m" my-mas-pair-toggle-mount :transient t
     :description
     (lambda () (format "%-12s %s" "  mount"
                        (my-mas--pair-value (my-mas--pair-get :mount)))))]
   ["Review rounds"
    ("-p" my-mas-pair-set-plan-rounds :transient t
     :description
     (lambda () (format "%-6s %s" "Plan"
                        (my-mas--pair-value
                         (number-to-string
                          (alist-get 'plan (my-mas--pair-get :limits)))))))
    ("-c" my-mas-pair-set-code-rounds :transient t
     :description
     (lambda () (format "%-6s %s" "Code"
                        (my-mas--pair-value
                         (number-to-string
                          (alist-get 'code (my-mas--pair-get :limits)))))))]]
  [["Run"
    ("C-c RET" "Start" my-mas-pair-start)
    ("D" "Reset to defaults" my-mas-pair-reset :transient t)]])

;;;###autoload
(defun my-mas-status ()
  "Show the state of the current multi-agent run."
  (interactive)
  (let ((run (my-mas--current-run)))
    (message "MAS %s: %s, step %s, round %d/%d, %s elapsed%s"
             (my-mas-run-id run) (my-mas--state-label run)
             (my-mas-run-step run) (my-mas-run-n run)
             (my-mas--max run) (my-mas--elapsed run)
             (if (my-mas-run-reason run)
                 (format " -- %s" (my-mas-run-reason run))
               ""))))

;;;###autoload
(defun my-mas-continue ()
  "Continue the current run past a gate, a pause or a discussion.
A discussion with choices (the plan presentation) asks what comes
next.  While an agent is still working, offer to re-send its step
prompt."
  (interactive)
  (let ((run (my-mas--current-run)))
    (pcase (my-mas-run-status run)
      ('discuss (my-mas--end-discussion run))
      ((or 'gate 'paused)
       (let ((resume (my-mas-run-pending run)))
         (setf (my-mas-run-pending run) nil
               (my-mas-run-status run) 'running
               (my-mas-run-reason run) nil)
         (when resume (funcall resume))))
      ('running
       (when-let* ((waiting (my-mas-run-waiting run))
                   ((y-or-n-p (format "%s is still on %s; re-send the step? "
                                      (my-mas--role-label
                                       run (plist-get waiting :role))
                                      (plist-get waiting :step)))))
         (my-mas--send run (plist-get waiting :step))))
      (status (message "MAS %s: %s, nothing to continue"
                       (my-mas-run-id run) status)))))

;;;###autoload
(defun my-mas-pause ()
  "Pause the current run after the agent's current turn."
  (interactive)
  (let ((run (my-mas--current-run)))
    (if (eq (my-mas-run-status run) 'running)
        (progn
          (setf (my-mas-run-pause-requested run) t)
          (message "MAS %s: pauses after the current turn"
                   (my-mas-run-id run)))
      (message "MAS %s: %s" (my-mas-run-id run) (my-mas-run-status run)))))

;;;###autoload
(defun my-mas-abort ()
  "Abort the current run: stop both agents and remove their containers."
  (interactive)
  (let ((run (my-mas--current-run)))
    (when (y-or-n-p (format "Abort MAS run %s and stop its agents? "
                            (my-mas-run-id run)))
      (if (my-mas--finished-p run)
          (my-mas--stop-agents run)
        (my-mas--finish run 'aborted "aborted by the operator" t)))))

;;;###autoload
(defun my-mas-open-run ()
  "Open the folder (or report) of the current or latest run."
  (interactive)
  (let* ((run (ignore-errors (my-mas--current-run)))
         (dir (if run (my-mas-run-dir run)
                (let* ((root (my-dsh-acp--project-root))
                       (runs (expand-file-name ".mas" root)))
                  (unless (file-directory-p runs)
                    (user-error "MAS: no runs in %s" root))
                  runs)))
         (report (expand-file-name "report.org" dir)))
    (find-file (if (file-exists-p report) report dir))))

;;;###autoload
(defun my-mas-layout ()
  "Show the current run's agents side by side again."
  (interactive)
  (my-mas--layout (my-mas--current-run)))

(transient-define-prefix my-mas-menu ()
  "Multi-agent runs (Claude / Codex / DSH)."
  [["Start"
    ("o" "openspec change" my-mas-openspec)
    ("a" "architecture decision" my-mas-architecture)
    ("i" "implementer + reviewer" my-mas-pair-menu)]
   ["Current run"
    ("s" "Status" my-mas-status)
    ("c" "Continue" my-mas-continue)
    ("p" "Pause after this turn" my-mas-pause)
    ("k" "Abort (stop agents)" my-mas-abort)
    ("l" "Side-by-side layout" my-mas-layout)
    ("r" "Open run folder / report" my-mas-open-run)]])

(provide 'desktop-mas)
;;; desktop-mas.el ends here
