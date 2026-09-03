;;; desktop-roguelike.el --- ASCII roguelike copilot (NetHack) for the Desktop -*- lexical-binding: t; -*-

;;; Commentary:
;; A "pocket wikibook" copilot for terminal roguelikes (NetHack
;; first), living next to a ghostel terminal: game on the left, gptel
;; chat on the right, Super-prefixed commands on top.  The human
;; plays every move; the copilot only reads and answers.
;;
;; What the copilot sees (all read-only over the session):
;;   - the ghostel viewport: the literal screen, with the cursor
;;     position annotated;
;;   - a message log: every message line the game displayed, appended
;;     as "T<turn>: <message>" (the game is turn-based, so sampling
;;     the top row after each keystroke loses nothing visible);
;;   - compaction memory: every ~my-desktop-roguelike-log-chunk-size
;;     log lines an async request rewrites a durable "state card"
;;     (identifications, intrinsics, pet status, discoveries) and
;;     appends a chapter summary.
;;
;; Every advisor question is answered from: state card + last chapter
;; summaries + last N raw log lines + parsed status + screen.
;;
;; Keymap (Super prefix, active only in game terminal buffers):
;;   s-d describe state   s-o describe object under game cursor
;;   s-a advice           s-q ask (goes to the chat buffer)
;;   s-h explain recent messages
;;   s-t story: turn the raw log into a chronicle (works after death)
;;   s-c focus the chat buffer
;;
;; The read_screen tool is offered only to the copilot: the session
;; chat buffer and one-shot advisor questions add it to gptel's
;; tools; regular gptel chats never see it.
;;
;; Files live under `my-desktop-roguelike-log-dir' (git-ignored):
;;   <session>/game.log        raw T<turn>: message lines
;;   <session>/chunk-NNN.md    async compaction summaries
;;   <session>/state-card.md   durable facts, rewritten by compaction
;;   <session>/story.org       written by `my-roguelike-story'

;;; Code:

(require 'desktop-config-defs)

;; Externals: ghostel variables are buffer-local in terminal buffers;
;; gptel variables come from gptel.el / gptel-request.el.
(defvar ghostel--term)
(defvar ghostel--cursor-pos)
(defvar gptel--system-message)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-model)

(declare-function ghostel--viewport-start "ghostel")
(declare-function ghostel--send-string "ghostel")
(declare-function gptel-mode "gptel")
(declare-function gptel-send "gptel")
(declare-function gptel-request "gptel-request")
(declare-function gptel-make-tool "gptel-request")
(declare-function my-desktop--warn "desktop-init")
(declare-function my-term-new "desktop-term")
(defvar my-desktop--warnings)


;; ------------------------------------------------------------------
;; Prompt knowledge (profiles, compaction, story)
;; ------------------------------------------------------------------

(defconst my-roguelike--nethack-directive
  "You are the player's pocket guide for NetHack 3.6 (tty interface).
The human plays every move; you never play.  You explain what they
see, warn about dangers, and lay out options.

Screen format (see the SCREEN block in the context):
- First line: the latest message line.
- Middle: the map.  @ is the player; letters are monsters; items:
  ) weapon  [ armor  % food  ? scroll  ! potion  / wand  = ring  *
  gem  ( tool  + spellbook and closed door  \" amulet  $ gold;
  ^ trap  > < stairs  _ altar  { fountain  \\ throne  | - walls
  . floor  ' open door.  Pets are lowercase letters, usually near @.
- Last 2-3 lines: status: Dlvl depth, $ gold, HP:cur(max), Pw:cur(max),
  AC, XL/Exp level, T turn count; prolonged conditions (Hunger,
  Satiated, Conf, Stun, Blind, Lev, Burdened, ...) are spelled out.
- The cursor position is given below the map.  In NetHack the cursor
  usually sits on the player, and on the inspected square after the
  far-look command ';'.

Rules:
- Lead the answer with anything acutely dangerous (low HP trend,
  harmful status, hostile neighbors, pet in danger, cursed gear in
  use, an unidentified item about to be used).
- Never invent screen contents.  Read only the context or your tool
  results; if unsure, say so and suggest an in-game check (far look
  ';', what-is '/', inventory 'i', discoveries '\\\\', ^X for stats).
- Exact numbers (prices, enchantments, probabilities, resistances):
  give them when sure; otherwise describe the mechanism and mark the
  number as approximate -- a confident wrong number kills characters.
- Concise by default: verdict, reason, what to do.  Deeper mechanics
  on follow-up questions.
- You are a wikibook, not a censor: explain mechanics fully when
  asked, including spoilers; narrating unexplored dungeon content is
  the player's business, not yours.
- Answer in the language of the question (default English).")

(defconst my-roguelike--angband-directive
  "You are the player's pocket guide for Angband (tty interface).
The human plays every move; you never play.  You explain what they
see, warn about dangers, and lay out options.  The map uses @ for
the player, letters for monsters, and punctuation for objects; the
bottom lines are the status bars (depth, HP, SP, stats) and the
message/history area is near the bottom of the screen.  Rules: lead
with anything acutely dangerous; never invent screen contents;
approximate numbers must be marked as approximate; concise by
default; answer in the language of the question.")

(defconst my-roguelike--profiles
  `(("nethack" . (:label "NetHack"
                         :directive ,my-roguelike--nethack-directive))
    ("angband" . (:label "Angband"
                         :directive ,my-roguelike--angband-directive)))
  "Copilot profiles keyed by the :profile setting of a game.")

(defconst my-roguelike--compactor-directive
  "You maintain the running memory of a NetHack advisor.

Input: the current STATE CARD and a NEW EVENTS fragment of a raw
play log (lines are 'T<turn>: <what the player saw>').

Reply with EXACTLY two org sections, in this order:

## SUMMARY
A chronological narrative of THIS fragment only, 120 words max.

## STATE CARD
The updated durable-facts card as an org list: a superset of the
input card, merged with new facts from the fragment.

Rules:
- Preserve exact facts verbatim: item identifications and BUC
  state, intrinsics with the T when gained, pet name/species/status,
  shop, altar and level discoveries, wishes, quests, key curses,
  prices that identified an item.
- Drop transient noise (melee spam, hunger ticks) unless notable.
- Never invent anything not present in the card or the fragment.
- Keep the card under 25 lines; drop superseded entries (pet status
  changes, items lost or destroyed).")

(defconst my-roguelike--chapter-directive
  "You summarize a fragment of a NetHack play log into one factual
chapter of a chronicle.  Preserve, in order: character events, item
finds and identifications, level and branch milestones, pet events,
notable kills and near-deaths, turn numbers for key moments.  120
words max.  No commentary, no invention: only what the log says.")

(defconst my-roguelike--story-directive
  "You are a chronicler turning a NetHack character's play log into
their story.  You receive the state card and factual chapter
summaries.  Write a flowing narrative (500-900 words) of the
character's life: origin and starting kit, rising fortunes,
discoveries, turning points, and the end -- how they died or
ascended.  Quote the log's best lines sparingly.  Be specific
(names, items, turn numbers); invent nothing; close with a one-line
epitaph.")

(defconst my-roguelike--oneshot-note
  "This is a one-shot question: the reply appears in a small popup,
so be compact (under 150 words unless detail was requested).")

;; Message-line suffix NetHack uses when it waits for a keypress.
(defconst my-roguelike--more-regex "\\(?:--More--\\|-More-\\)\\'")

;; Prolonged conditions spelled out in the 3.6 status area.
(defconst my-roguelike--cond-regex
  "\\b\\(Satiated\\|Hungry\\|Weak\\|Fainting\\|Conf\\|Stun\\|Blind\\|Lev\\|Fly\\|Fear\\|Energetic\\|Fatigued\\|Sleepy\\|Hallu\\|Glib\\|Slippery\\|Food poisoning\\|Terminally ill\\|Burdened\\|Stressed\\|Strained\\|Overted\\|Overloaded\\|Fumbling\\)\\b")


;; ------------------------------------------------------------------
;; Session state
;; ------------------------------------------------------------------

(defvar my-roguelike--current nil
  "Plist describing the active (or last) roguelike session.
Keys: :game :profile :dir :log :card :buffer :chat :command
:lines :compacted :chunks :compacting :last-compact :last-msg
:over :over-captures :hp-ring.")

(defvar my-roguelike--read-screen-tool nil
  "The read_screen gptel tool, created once when gptel loads.
Only copilot contexts (the session chat buffer and one-shot
advisor requests) add it to `gptel-tools'; it never enters the
global default tool list, so regular gptel chats don't see it.")

(defun my-roguelike--session-or-error ()
  "Return the current session plist or error out."
  (or my-roguelike--current
      (user-error "No roguelike session; run M-x my-roguelike-start")))

(defun my-roguelike--log-dir ()
  "Session root directory, created on demand."
  (let ((dir (expand-file-name my-desktop-roguelike-log-dir)))
    (make-directory dir t)
    dir))


;; ------------------------------------------------------------------
;; File helpers
;; ------------------------------------------------------------------

(defun my-roguelike--read-file (file)
  "Return FILE contents as a string, or nil when missing."
  (and file (file-exists-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))

(defun my-roguelike--write-file (file text)
  "Write TEXT to FILE, creating parent directories."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (insert text)
    (write-region (point-min) (point-max) file nil 'silent)))

(defun my-roguelike--file-lines (file)
  "Return FILE as a list of non-empty lines, or nil."
  (let ((text (my-roguelike--read-file file)))
    (and text (split-string text "\n" t))))

(defun my-roguelike--append-log (sess line)
  "Append LINE to the session log file."
  (let ((file (plist-get sess :log)))
    (with-temp-buffer
      (insert line "\n")
      (append-to-file (point-min) (point-max) file)))
  (plist-put sess :lines (1+ (plist-get sess :lines))))

(defun my-roguelike--chunk-files (sess)
  "Session chunk summary files, oldest first."
  (let ((dir (plist-get sess :dir)))
    (when (and dir (file-directory-p dir))
      (sort (directory-files dir t "^chunk-[0-9]+\\.md\\'")
            #'string<))))

(defun my-roguelike--summaries-text (sess)
  "Last K chunk summaries as one string."
  (mapconcat
   (lambda (f) (string-trim (or (my-roguelike--read-file f) "")))
   (last (my-roguelike--chunk-files sess)
         my-desktop-roguelike-summary-count)
   "\n\n"))

(defun my-roguelike--log-slice (sess from to)
  "Log lines FROM (inclusive) to TO (exclusive)."
  (let ((all (my-roguelike--file-lines (plist-get sess :log))))
    (when (and all (< from (length all)))
      (seq-subseq all from (min to (length all))))))

(defun my-roguelike--recent-session ()
  "Session plist reconstructed from the newest session directory."
  (let* ((root (my-roguelike--log-dir))
         (cands (sort (directory-files root t "^[0-9]") #'string>))
         (dir (seq-find
               (lambda (d)
                 (file-exists-p (expand-file-name "game.log" d)))
               cands)))
    (when dir
      (list :game (file-name-nondirectory (directory-file-name dir))
            :dir dir
            :log (expand-file-name "game.log" dir)
            :card (expand-file-name "state-card.md" dir)
            :buffer nil :over t :hp-ring nil
            :profile (or (assoc-default "nethack" my-roguelike--profiles)
                         '(:label "game"))))))


;; ------------------------------------------------------------------
;; Pure helpers: status parsing, collapsing, formatting
;; ------------------------------------------------------------------

(defun my-roguelike--parse-status (lines)
  "Parse NetHack status fields out of viewport LINES.
Returns a plist: :hp :hpmax :pw :ac :dlvl :gold :xl :turn :conds
:block (raw status lines joined, string)."
  (let* ((block (string-join
                 (seq-filter
                  (lambda (l)
                    (string-match-p
                     "Dlvl:\\|HP:\\|Pw:\\|AC:\\|Exp:\\|XL:\\|T:[0-9]\\|Str:" l))
                  lines)
                 " | "))
         (field
          (lambda (re idx)
            (and (string-match re block)
                 (match-string idx block)))))
    (list
     :block block
     :hp (and (funcall field "HP:\\([0-9]+\\)" 1)
              (string-to-number (funcall field "HP:\\([0-9]+\\)" 1)))
     :hpmax (and (funcall field "HP:\\([0-9]+\\)(\\([0-9]+\\))" 2)
                 (string-to-number
                  (funcall field "HP:\\([0-9]+\\)(\\([0-9]+\\))" 2)))
     :pw (and (funcall field "Pw:\\([0-9]+\\)" 1)
              (string-to-number (funcall field "Pw:\\([0-9]+\\)" 1)))
     :ac (and (funcall field "AC:\\(-?[0-9]+\\)" 1)
              (string-to-number (funcall field "AC:\\(-?[0-9]+\\)" 1)))
     :dlvl (and (funcall field "Dlvl:\\([0-9]+\\)" 1)
                (string-to-number (funcall field "Dlvl:\\([0-9]+\\)" 1)))
     :gold (and (funcall field "\\$:\\([0-9]+\\)" 1)
                (string-to-number (funcall field "\\$:\\([0-9]+\\)" 1)))
     :xl (and (funcall field "\\(?:XL\\|Exp\\):\\([0-9]+\\)" 1)
              (string-to-number
               (funcall field "\\(?:XL\\|Exp\\):\\([0-9]+\\)" 1)))
     :turn (and (funcall field "T:\\([0-9]+\\)" 1)
                (string-to-number (funcall field "T:\\([0-9]+\\)" 1)))
     :conds (my-roguelike--conds block))))

(defun my-roguelike--conds (block)
  "Collect status condition words out of BLOCK."
  (let ((pos 0) out)
    (while (string-match my-roguelike--cond-regex block pos)
      (push (match-string 1 block) out)
      (setq pos (match-end 0)))
    (setq out (delete-dups out))
    (string-join (nreverse out) ", ")))

(defun my-roguelike--collapse-dups (lines)
  "Collapse runs of identical adjacent LINES into one [xN] line."
  (let (out)
    (while lines
      (let ((line (car lines)) (run 1))
        (while (and (cdr lines) (string= (cadr lines) line))
          (setq run (1+ run)
                lines (cdr lines)))
        (push (if (> run 1) (format "%s  [x%d]" line run) line) out)
        (setq lines (cdr lines))))
    (nreverse out)))

(defun my-roguelike--hp-trend (ring)
  "Recent HP values, oldest first, as '34 -> 21 -> 9' or nil."
  (when (> (length ring) 1)
    (mapconcat (lambda (p) (number-to-string (cdr p)))
               (reverse (seq-take ring 8)) " -> ")))

(defun my-roguelike--format-screen (lines cursor)
  "Render viewport LINES + CURSOR as a labeled block for the model."
  (concat
   (format "SCREEN (%d rows x ~%d cols)"
           (length lines) (length (car lines)))
   (when cursor
     (format ", game cursor at row %d col %d"
             (car cursor) (cadr cursor)))
   "\n"
   (mapconcat #'identity lines "\n")
   "\n[/SCREEN]"))

(defun my-roguelike--chunk-text (lines size)
  "Join LINES into chunks of about SIZE characters, line-aligned."
  (let (chunks cur)
    (dolist (l lines)
      (setq cur (if cur (concat cur "\n" l) l))
      (when (>= (length cur) size)
        (push cur chunks)
        (setq cur nil)))
    (when cur (push cur chunks))
    (nreverse chunks)))

(defun my-roguelike--split-compaction (reply)
  "Split a compactor REPLY into (SUMMARY . STATE-CARD)."
  (let ((case-fold-search t) sum-start card-start)
    (save-match-data
      (when (string-match "^#+.*SUMMARY.*$" reply)
        (setq sum-start (match-end 0)))
      (when (string-match "^#+.*STATE CARD.*$" reply)
        (setq card-start (match-beginning 0)))
      (cond
       ((and sum-start card-start)
        (cons (string-trim (substring reply sum-start card-start))
              (string-trim (substring reply card-start))))
       (sum-start
        (cons (string-trim (substring reply sum-start)) nil))
       (card-start
        (cons nil (string-trim (substring reply card-start))))
       (t (cons (string-trim reply) nil))))))


;; ------------------------------------------------------------------
;; Screen capture
;; ------------------------------------------------------------------

(defun my-roguelike--viewport-lines ()
  "In the game buffer: the visible screen as trimmed lines."
  (when-let* ((start (ghostel--viewport-start)))
    (save-excursion
      (goto-char start)
      (let (lines)
        (while (< (point) (point-max))
          (push (string-trim-right
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
                lines)
          (forward-line 1))
        (nreverse lines)))))

(defun my-roguelike--snapshot ()
  "In the game buffer: (:lines :cursor :status :digest)."
  (let* ((lines (my-roguelike--viewport-lines))
         (cursor (and (boundp 'ghostel--cursor-pos) ghostel--cursor-pos))
         (status (my-roguelike--parse-status lines)))
    (list :lines lines
          :cursor (and cursor
                       (list (1+ (cdr cursor)) (1+ (car cursor))))
          :status status
          :digest (my-roguelike--digest
                   status (plist-get my-roguelike--current :hp-ring)))))

(defun my-roguelike--digest (status ring)
  "One-line digest from parsed STATUS + HP RING."
  (let ((block (plist-get status :block)))
    (if (or (string-empty-p block)
            (not (string-match-p "HP:" block)))
        "(no status line visible)"
      (concat block
              (let ((trend (my-roguelike--hp-trend ring)))
                (and trend (concat "\nHP trend (recent): " trend)))))))

(defun my-roguelike--track-hp (sess snap)
  "Record (turn . hp) in the session ring when it changes."
  (let* ((st (plist-get snap :status))
         (hp (plist-get st :hp))
         (turn (plist-get st :turn))
         (ring (plist-get sess :hp-ring)))
    (when (and hp turn
               (or (not ring) (/= hp (cdar ring))))
      (plist-put sess :hp-ring
                 (seq-take (cons (cons turn hp) ring) 30)))))

(defun my-roguelike--sample ()
  "Post-command-hook: log the message line, watch for game over."
  (let ((sess my-roguelike--current))
    (when (and sess (eq (current-buffer) (plist-get sess :buffer)))
      (condition-case err
          (let* ((snap (my-roguelike--snapshot))
                 (lines (plist-get snap :lines))
                 (raw (or (car lines) ""))
                 (msg (string-trim
                       (if (string-match my-roguelike--more-regex raw)
                           (substring raw 0 (match-beginning 0))
                         raw)))
                 (turn (plist-get (plist-get snap :status) :turn)))
            (when (and (not (string-empty-p msg))
                       (not (equal msg (plist-get sess :last-msg))))
              (plist-put sess :last-msg msg)
              (my-roguelike--append-log
               sess (format "T%s: %s" (or turn "?") msg)))
            (my-roguelike--track-hp sess snap)
            (unless (plist-get sess :over)
              (when (string-match-p "possessions identified" raw)
                (plist-put sess :over t)
                (my-roguelike--append-log
                 sess "=== GAME OVER: final screens follow ===")))
            (when (plist-get sess :over)
              (let ((n (plist-get sess :over-captures)))
                (when (< n 8)
                  (plist-put sess :over-captures (1+ n))
                  (my-roguelike--append-log
                   sess (concat "=== SCREEN ===\n"
                                (my-roguelike--format-screen lines nil)
                                "\n=== END SCREEN ===")))))
            (my-roguelike--maybe-compact sess))
        (error (my-desktop--warn
                "roguelike sampler: %s" (error-message-string err)))))))

(defun my-roguelike--on-exit ()
  "Game buffer was killed (game ended)."
  (let ((sess my-roguelike--current))
    (when sess
      (plist-put sess :buffer nil)
      (message "[roguelike] game ended.  Log: %s  (story: M-x my-roguelike-story)"
               (plist-get sess :log)))))


;; ------------------------------------------------------------------
;; Compaction (async, event-driven)
;; ------------------------------------------------------------------

(defun my-roguelike--maybe-compact (sess)
  "Compact the next chunk when enough lines are pending."
  (let ((pending (- (plist-get sess :lines)
                    (plist-get sess :compacted))))
    (when (and (>= pending my-desktop-roguelike-log-chunk-size)
               (not (plist-get sess :compacting))
               (> (- (float-time) (or (plist-get sess :last-compact) 0))
                  5.0))
      (my-roguelike--compact sess))))

(defun my-roguelike--compact (sess)
  "Send the next log chunk to the compactor, asynchronously."
  (let* ((start (plist-get sess :compacted))
         (end (+ start my-desktop-roguelike-log-chunk-size))
         (lines (my-roguelike--log-slice sess start end)))
    (when lines
      (plist-put sess :compacting t)
      (plist-put sess :last-compact (float-time))
      (my-roguelike--model-request
       (my-roguelike--compaction-prompt sess lines)
       my-roguelike--compactor-directive
       (lambda (resp)
         (plist-put sess :compacting nil)
         (if (stringp resp)
             (my-roguelike--apply-compaction sess end resp)
           (my-desktop--warn "roguelike compaction failed: %S" resp)
           (message "[roguelike] compaction failed (will retry)")))
       nil))))

(defun my-roguelike--compaction-prompt (sess lines)
  "Build the compactor prompt: card + NEW EVENTS fragment."
  (format "CURRENT STATE CARD:\n%s\n\nNEW EVENTS (%d lines):\n%s"
          (or (my-roguelike--read-file (plist-get sess :card))
              "(empty -- this is the first compaction)")
          (length lines)
          (string-join lines "\n")))

(defun my-roguelike--apply-compaction (sess end reply)
  "Store the compactor REPLY: summary chunk + updated state card."
  (let* ((split (my-roguelike--split-compaction reply))
         (summary (car split))
         (card (cdr split))
         (n (1+ (plist-get sess :chunks)))
         (file (expand-file-name (format "chunk-%03d.md" n)
                                 (plist-get sess :dir))))
    (when (and summary (not (string-empty-p summary)))
      (my-roguelike--write-file file summary)
      (plist-put sess :chunks n)
      (plist-put sess :compacted end))
    (when (and card (not (string-empty-p card)))
      (my-roguelike--write-file (plist-get sess :card) card))
    (message "[roguelike] compacted chunk %d" n)))


;; ------------------------------------------------------------------
;; gptel plumbing
;; ------------------------------------------------------------------

(defun my-roguelike--model-request (prompt system callback &optional tools)
  "Run an async gptel request: PROMPT + SYSTEM -> CALLBACK.
CALLBACK receives the response string, or nil on failure.  TOOLS
enables gptel tools (read_screen, web search, ...) for the request;
disabled by default for mechanical requests (compaction, story)."
  (require 'gptel)
  (unless (bound-and-true-p gptel-backend)
    (user-error "No gptel backend; check the desktop AI settings"))
  (let ((gptel-model (or my-desktop-roguelike-model gptel-model))
        (gptel-use-tools (and tools t))
        (gptel-tools (and tools
                          (my-roguelike--tools-with-read-screen))))
    (condition-case err
        (gptel-request prompt
          :system system
          :callback (lambda (resp _info)
                      (funcall callback (and (stringp resp) resp))))
      (error (my-desktop--warn
              "roguelike request: %s" (error-message-string err))
             (funcall callback nil)))))

(defun my-roguelike--context (&optional recent)
  "Assemble the advisor context for the current session.
State card, recent chapter summaries, raw log tail, status digest,
screen."
  (let* ((sess (my-roguelike--session-or-error))
         (snap (when (buffer-live-p (plist-get sess :buffer))
                 (with-current-buffer (plist-get sess :buffer)
                   (my-roguelike--snapshot))))
         (loglines (my-roguelike--file-lines (plist-get sess :log)))
         (tail (mapconcat
                #'identity
                (my-roguelike--collapse-dups
                 (last loglines
                       (or recent my-desktop-roguelike-recent-lines)))
                "\n"))
         (summaries (my-roguelike--summaries-text sess))
         (card (or (my-roguelike--read-file (plist-get sess :card))
                   "(nothing compacted yet)")))
    (concat
     (format "GAME: %s%s\n"
             (plist-get sess :game)
             (if (plist-get sess :over) "  [GAME OVER]" ""))
     "\n== DURABLE STATE CARD ==\n" card "\n"
     (if (string-empty-p summaries) ""
       (concat "\n== EARLIER EVENTS (chapter summaries) ==\n"
               summaries "\n"))
     "\n== RECENT MESSAGES ==\n"
     (if (string-empty-p tail) "(none)" tail)
     "\n\n== CURRENT STATUS ==\n"
     (or (and snap (plist-get snap :digest)) "(game not running)")
     (when snap
       (concat "\n\n"
               (my-roguelike--format-screen
                (plist-get snap :lines) (plist-get snap :cursor)))))))

(defun my-roguelike--register-tool ()
  "Create the copilot's read_screen tool (once, after gptel loads).
The tool is kept out of the global `gptel-tools': copilot contexts
add it locally, so regular gptel chats never see it."
  (unless my-roguelike--read-screen-tool
    (when (fboundp 'gptel-make-tool)
      (setq my-roguelike--read-screen-tool
            (gptel-make-tool
             :function #'my-roguelike--tool-read-screen
             :name "read_screen"
             :description
             "Read the current screen of the user's NetHack \
roguelike session: status digest, the ASCII map, and the last \
messages the game displayed.  Use it to see what the player sees \
right now."
             :args (list '(:name "messages"
                                 :type integer
                                 :optional t
                                 :description
                                 "how many recent log lines \
to include (default 40)"))
             :category "roguelike")))))

(defun my-roguelike--tools-with-read-screen ()
  "Default gptel tools plus the copilot's read_screen tool."
  (append (default-value 'gptel-tools)
          (when my-roguelike--read-screen-tool
            (list my-roguelike--read-screen-tool))))

(defun my-roguelike--tool-read-screen (&optional messages)
  "read_screen tool body: context for the live session."
  (let ((sess my-roguelike--current))
    (if (not sess)
        "No roguelike session is active."
      (let* ((n (max 1 (min 400 (or messages 40))))
             (ctx (my-roguelike--context n)))
        (if (> (length ctx) 16000)
            (substring ctx 0 16000)
          ctx)))))

;; Create the read_screen tool whenever gptel loads (after
;; desktop-ai has built its tool list), or right away if gptel is
;; already loaded.  It is NOT added to `gptel-tools' globally.
(with-eval-after-load 'gptel (my-roguelike--register-tool))
(when (featurep 'gptel) (my-roguelike--register-tool))


;; ------------------------------------------------------------------
;; Answers
;; ------------------------------------------------------------------

(defun my-roguelike--show-answer (title text)
  "Show TEXT in a small popup buffer titled TITLE."
  (let* ((buf (get-buffer-create "*roguelike:answer*"))
         (cfg (current-window-configuration)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* %s\n\n" title) text "\n")
        (goto-char (point-min))
        (setq buffer-read-only t)
        (local-set-key
         "q" (lambda () (interactive)
               (set-window-configuration cfg)))))
    (pop-to-buffer buf '(display-buffer-below-selected
                         (window-height . 0.45)))
    (message "[roguelike] q closes the answer")))

(defun my-roguelike--one-shot (title template &optional recent)
  "Ask the model TEMPLATE with assembled context; popup the answer."
  (let* ((sess (my-roguelike--session-or-error))
         (directive (or (plist-get (plist-get sess :profile) :directive)
                        "You are a game advisor."))
         (ctx (my-roguelike--context recent)))
    (message "[roguelike] %s: asking the model..." title)
    (my-roguelike--model-request
     (concat template "\n\n" ctx)
     (concat directive "\n\n" my-roguelike--oneshot-note)
     (lambda (resp)
       (if resp
           (my-roguelike--show-answer title resp)
         (message "[roguelike] request failed (see *Warnings*)")))
     t)))


;; ------------------------------------------------------------------
;; Interactive commands
;; ------------------------------------------------------------------

;;;###autoload
(defun my-roguelike-describe-state ()
  "Describe the current game situation."
  (interactive)
  (my-roguelike--one-shot
   "State"
   "Describe my current situation.  Cover: (1) immediate threats,
(2) my pet if visible, (3) status flags worth attention, (4)
anything on the message line I may have missed, (5) what likely
changed since the earlier logged events.  Lead with danger."))

;;;###autoload
(defun my-roguelike-describe-object ()
  "Explain what the game cursor points at."
  (interactive)
  (my-roguelike--one-shot
   "Object"
   "Explain what is at the game cursor position given in the SCREEN
header: what it is, its mechanics, threat level, and what to do
about it.  If the cursor rests on my character, explain my
immediate surroundings and the nearest notable things instead."))

;;;###autoload
(defun my-roguelike-advise ()
  "Ask for the best next moves."
  (interactive)
  (my-roguelike--one-shot
   "Advice"
   "Given everything in the context, give me the 2-3 best next
moves with their risks and payoffs, and recommend one.  Consider my
longer-term position from the state card, not only the visible
screen."))

;;;###autoload
(defun my-roguelike-explain-last ()
  "Explain the recent game messages."
  (interactive)
  (my-roguelike--one-shot
   "What just happened"
   "Explain my recent messages: what happened, what it means
mechanically, and whether I should react to any of it."
   200))

;;;###autoload
(defun my-roguelike-ask (question)
  "Ask QUESTION in the session chat buffer, with screen context.
The full conversation lives in the chat buffer (auto-saved as an
AI session); the copilot can also call its read_screen tool."
  (interactive "MAsk the roguelike copilot: ")
  (let* ((sess (my-roguelike--session-or-error))
         (chat (plist-get sess :chat)))
    (unless (buffer-live-p chat)
      (user-error "Chat buffer is gone; run M-x my-roguelike-start"))
    (with-current-buffer chat
      (goto-char (point-max))
      (insert (format "\n* %s\n\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n\n"
                      question (my-roguelike--context 40)))
      (condition-case err
          (gptel-send)
        (error (message "[roguelike] gptel-send: %s"
                        (error-message-string err)))))
    (message "[roguelike] sent; the answer streams into %s"
             (buffer-name chat))))

;;;###autoload
(defun my-roguelike-chat ()
  "Focus the session chat buffer."
  (interactive)
  (let* ((sess (my-roguelike--session-or-error))
         (chat (plist-get sess :chat)))
    (if (buffer-live-p chat)
        (pop-to-buffer chat)
      (user-error "Chat buffer is gone"))))

;;;###autoload
(defun my-roguelike-story ()
  "Turn the session log into a chronicle of the character's life.
Re-reads the raw log, summarizes it chapter by chapter, then writes
the story to <session>/story.org.  Works after the game ended."
  (interactive)
  (let* ((sess (or my-roguelike--current
                   (my-roguelike--recent-session)))
         (lines (my-roguelike--file-lines (plist-get sess :log))))
    (unless (and lines (> (length lines) 3))
      (user-error "Session log is empty or missing"))
    (let* ((chunks (my-roguelike--chunk-text
                    (my-roguelike--collapse-dups lines) 12000))
           (acc (make-vector (length chunks) nil)))
      (message "[roguelike] story: %d chapters" (length chunks))
      (my-roguelike--story-step sess chunks 0 acc))))

(defun my-roguelike--story-step (sess chunks idx acc)
  "Summarize chapter IDX, then continue the chain."
  (if (>= idx (length chunks))
      (my-roguelike--story-final sess acc)
    (message "[roguelike] story: chapter %d/%d"
             (1+ idx) (length chunks))
    (my-roguelike--model-request
     (nth idx chunks)
     my-roguelike--chapter-directive
     (lambda (resp)
       (if (not (stringp resp))
           (message "[roguelike] story: chapter %d failed; run \
M-x my-roguelike-story to retry" (1+ idx))
         (aset acc idx resp)
         (my-roguelike--story-step sess chunks (1+ idx) acc)))
     nil)))

(defun my-roguelike--story-final (sess acc)
  "Synthesize the chapters into the final story."
  (let ((card (or (my-roguelike--read-file (plist-get sess :card))
                  "(no state card)"))
        (chapters
         (mapconcat
          (lambda (i)
            (format "* Chapter %d\n%s" (1+ i) (aref acc i)))
          (number-sequence 0 (1- (length acc))) "\n\n")))
    (message "[roguelike] story: writing the chronicle")
    (my-roguelike--model-request
     (concat "STATE CARD:\n" card "\n\nCHAPTERS:\n" chapters)
     my-roguelike--story-directive
     (lambda (resp)
       (if (not (stringp resp))
           (message "[roguelike] story: synthesis failed; retry")
         (let* ((file (expand-file-name
                       "story.org" (plist-get sess :dir)))
                (buf (get-buffer-create "*roguelike:story*")))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (org-mode)
               (insert (format "* The story of %s\n\n"
                               (plist-get sess :game))
                       resp "\n")
               (write-region (point-min) (point-max) file nil 'silent)))
           (pop-to-buffer buf)
           (message "[roguelike] story saved to %s" file)))))))

;;;###autoload
(defun my-roguelike-stop ()
  "Detach the copilot from the current session (files are kept)."
  (interactive)
  (let ((sess my-roguelike--current))
    (when (and sess (buffer-live-p (plist-get sess :buffer)))
      (with-current-buffer (plist-get sess :buffer)
        (my-roguelike-mode -1)
        (remove-hook 'kill-buffer-hook #'my-roguelike--on-exit t)))
    (setq my-roguelike--current nil)
    (message "[roguelike] session detached")))


;; ------------------------------------------------------------------
;; Session start, layout, launch
;; ------------------------------------------------------------------

(defun my-roguelike--make-chat (game profile)
  "Create or reuse the chat buffer for GAME with PROFILE."
  (let* ((name (format "*roguelike:%s*" game))
         (buf (or (get-buffer name) (get-buffer-create name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (require 'gptel)
      (unless (bound-and-true-p gptel-mode)
        (gptel-mode 1))
      (setq-local gptel--system-message
                  (or (plist-get profile :directive) ""))
      ;; This buffer is the only gptel chat that gets the copilot's
      ;; read_screen tool; regular chats keep gptel's default tools.
      (setq-local gptel-tools
                  (my-roguelike--tools-with-read-screen))
      (when (= (buffer-size) 0)
        (insert (format "* %s copilot -- %s\n\n"
                        game (format-time-string "%Y-%m-%d %H:%M"))))
      buf)))

(defun my-roguelike--layout (term-buf chat-buf)
  "Game left, chat right, focus on the game."
  (delete-other-windows)
  (set-window-buffer (selected-window) term-buf)
  (set-window-buffer (split-window-right) chat-buf)
  (select-window (get-buffer-window term-buf)))

(defun my-roguelike--launch (buf cmd &optional tries)
  "Type CMD into the terminal buffer once its shell is ready."
  (run-with-timer
   1.0 nil
   (lambda ()
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (if (and (boundp 'ghostel--term) ghostel--term)
             (condition-case err
                 (progn
                   (ghostel--send-string (concat cmd "\r"))
                   (message "[roguelike] launched: %s" cmd))
               (error (my-desktop--warn
                       "roguelike launch: %s"
                       (error-message-string err))))
           (if (and tries (> tries 1))
               (my-roguelike--launch buf cmd (1- tries))
             (message "[roguelike] terminal not ready yet; \
type the command yourself: %s" cmd))))))))

;;;###autoload
(defun my-roguelike-start (&optional game)
  "Start a roguelike copilot session for GAME.
Lays out ghostel (left) and a gptel chat (right), enables the
Super keymap on the game buffer, and launches the game command."
  (interactive)
  (when (and my-roguelike--current
             (buffer-live-p (plist-get my-roguelike--current :buffer)))
    (user-error "A roguelike session is already running; \
M-x my-roguelike-stop first"))
  (unless (and (fboundp 'my-term-new) (featurep 'ghostel))
    (user-error "The ghostel terminal module is disabled"))
  (let* ((games (append my-desktop-roguelike-games nil)))
    (unless games
      (user-error "Configure `my-desktop-roguelike-games' first"))
    (setq game (or game
                   (completing-read
                    "Game: " (mapcar #'car games) nil t
                    (or my-desktop-roguelike-default-game ""))))
    (let* ((spec (or (assoc game games)
                     (user-error "Unknown game %s" game)))
           (cmd (or (plist-get (cdr spec) :command)
                    (user-error "No :command for %s" game)))
           (gdir (plist-get (cdr spec) :dir))
           (pkey (or (plist-get (cdr spec) :profile) (downcase game)))
           (profile (or (assoc-default pkey my-roguelike--profiles)
                        (assoc-default "nethack" my-roguelike--profiles)))
           (dir (expand-file-name
                 (concat (format-time-string "%Y%m%d-%H%M%S")
                         "-" (downcase game))
                 (my-roguelike--log-dir))))
      (make-directory dir t)
      (my-term-new (and gdir (expand-file-name gdir)))
      (let* ((buf (my-roguelike--term-buffer)))
        (unless buf
          (user-error "Could not find the ghostel terminal buffer"))
        (setq my-roguelike--current
              (list :game game
                    :profile profile
                    :dir dir
                    :log (expand-file-name "game.log" dir)
                    :card (expand-file-name "state-card.md" dir)
                    :buffer buf
                    :chat (my-roguelike--make-chat game profile)
                    :command cmd
                    :lines 0 :compacted 0 :chunks 0 :compacting nil
                    :last-compact 0 :last-msg nil
                    :over nil :over-captures 0 :hp-ring nil))
        (with-current-buffer buf
          (my-roguelike-mode 1)
          (add-hook 'kill-buffer-hook #'my-roguelike--on-exit nil t))
        (my-roguelike--layout
         buf (plist-get my-roguelike--current :chat))
        (my-roguelike--launch buf cmd 3)
        (message
         "[roguelike] %s session in %s  (s-d state | s-a advice | \
s-q ask | s-h what happened | s-t story | s-c chat)"
         game dir)))))

(defun my-roguelike--term-buffer ()
  "The ghostel buffer just opened by `my-term-new', or nil."
  (let ((buf (current-buffer)))
    (unless (with-current-buffer buf (derived-mode-p 'ghostel-mode))
      (setq buf (seq-find
                 (lambda (b)
                   (with-current-buffer b
                     (derived-mode-p 'ghostel-mode)))
                 (buffer-list))))
    buf))

;; The Super keymap itself lives here (this module loads before
;; desktop-keys.el, which binds the commands into it via the
;; `roguelike' context).
(defvar my-roguelike-keymap (make-sparse-keymap)
  "Super-prefixed copilot commands, active only in game terminal
buffers via `my-roguelike-mode'.")

;; Minor mode: hooks + keymap.
;;;###autoload
(define-minor-mode my-roguelike-mode
  "Roguelike copilot hooks for a ghostel game buffer."
  :init-value nil
  :lighter " RL"
  :keymap my-roguelike-keymap
  (if my-roguelike-mode
      (add-hook 'post-command-hook #'my-roguelike--sample nil t)
    (remove-hook 'post-command-hook #'my-roguelike--sample t)))

(provide 'desktop-roguelike)
;;; desktop-roguelike.el ends here
