;;; lang-metals-v2.el --- Metals v2 (2.0.0-M14) Eglot setup for Scala -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in Metals v2 configuration.  Loaded only when
;; `my-use-metals-v2' is non-nil (see `modules/lang-prog.el').
;;
;; It overrides the `scala-mode' entry in `eglot-server-programs' so
;; that Eglot launches `metals-v2' instead of the stable `metals' (v1).
;; All v1 client-side hooks (eglot-ensure, jarchive, build-import,
;; sbt-mode) are reused unchanged -- they are version-agnostic.
;;
;; Fallback: set `my-use-metals-v2' back to nil and restart Emacs.

;;; Code:

(require 'eglot)
(defvar eglot-server-programs)

;; --- User-tunable options ---------------------------------------------

(defcustom my-metals-v2-binary "metals-v2"
  "Launcher command for Metals v2.
Must be on PATH (or an absolute path)."
  :type 'string
  :group 'eglot)

(defcustom my-metals-v2-server-version "2.0.0-M14"
  "Metals server version requested via `initializationOptions'.
Matches `metals-v2 --version' (M14 at the time of writing)."
  :type 'string
  :group 'eglot)

;; --- JVM options ------------------------------------------------------
;; Metals v2 requires JDK internals access.  nvim-metals documents these
;; as `serverProperties' inside `initializationOptions'; if Metals PR
;; #767 has set them automatically they are still harmless to send.
;;
;; Stored as a vector of strings so Eglot serializes it directly to a
;; JSON array (`string[]'), matching Metals' `serverProperties' shape.

(defconst my-metals-v2-jvm-properties
  [ "-Djol.magicFieldOffset=true"
    "-Djol.tryWithSudo=true"
    "-Djdk.attach.allowAttachSelf"
    "--add-opens=java.base/java.nio=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.comp=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.main=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.model=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.resources=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED"
    "--add-exports=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
    "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
    "--add-opens=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
    "--add-opens=jdk.compiler/com.sun.tools.javac.comp=ALL-UNNAMED"
    "--add-opens=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED"
    "--add-opens=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED"
    "-XX:+DisplayVMOutputToStderr"
    "-Xlog:disable"
    "-Xlog:all=warning,gc=warning:stderr"]
  "JVM flags required by Metals v2 (JDK internals access).
Passed to the server via `serverProperties' in `initializationOptions'.")

;; --- Command builder --------------------------------------------------

(defun my-metals-v2-server-command ()
  "Return the Eglot command tuple used to launch Metals v2.
Shape: (BINARY :initializationOptions ...)."
  `(,my-metals-v2-binary
    :initializationOptions
    (:isHttpEnabled      t
     :serverVersion      ,my-metals-v2-server-version
     :serverProperties   ,my-metals-v2-jvm-properties)))

;; --- Registration (override v1) ---------------------------------------
;; Runs after v1's `add-to-list' (this file is `require'd from
;; lang-prog.el after v1 registers its entry), so the v2 entry wins.
;; We use `setf'/`alist-get' to *replace* the v1 scala-mode entry,
;; leaving exactly one entry -- easier to inspect when debugging.

(with-eval-after-load 'eglot
  (setf (alist-get 'scala-mode eglot-server-programs nil nil #'equal)
        (my-metals-v2-server-command)))

;; --- Debug fallback note ----------------------------------------------
;; If Metals fails to start because the JVM needs these flags *before*
;; Metals spawns its own process (e.g. attach/restart loops), an
;; alternative is to pass them on the launcher command line using the
;; `-J' prefix, e.g.:
;;
;;   `("metals-v2"
;;     "-J-Djol.magicFieldOffset=true"
;;     "-J--add-opens=java.base/java.nio=ALL-UNNAMED"
;;     ...)
;;
;; Try this only if `serverProperties' turns out to be insufficient.

(provide 'lang-metals-v2)
;;; lang-metals-v2.el ends here
