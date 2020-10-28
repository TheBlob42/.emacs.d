;;; gradlew-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gradlew" "gradlew.el" (0 0 0 0))
;;; Generated autoloads from gradlew.el

(transient-define-prefix gradlew-transient (gw-path gw-task) "\
The main entry point to configure and execute any Gradlew task." ["Debugging Options" (5 "-v" "Print version information" "--version") (3 "-s" "Print ou stacktrace" "--stacktrace") (3 "-S" "Print out FULL stacktrace" "--full-stacktrace") (6 "--Dg" "Debug Gradle client" "-Dorg.gradle.debug=true") (6 "--Dd" "Debug Gradle Daemon" "-Dorg.gradle.daemon.debug=true") (3 "--d" "Debug JVM" "--debug-jvm")] ["Logging options" (4 "-q" "Log errors only" "--quit") (4 "-w" "Log level warn" "--warn") (4 "-i" "Log level info" "--info") (4 "-d" "Log in debug mode" "--debug") (7 "--ll" gradlew-transient--log-level-argument) (7 "--lc" gradlew-transient--console-output-type) (7 "--lw" gradlew-transient--warning-mode)] ["Daemon options" (8 "--dd" "Use Gradle Daemon (default)" "--daemon") (8 "--dn" "Do not use the Gradle Daemon" "--no-daemon") (8 "--df" "Start Daemon in a foreground process" "--foreground")] ["Execution options" (5 "--eo" "Operating without network access" "--offline") (3 "--er" "Refresh the state of dependencies" "--refresh-dependencies") (6 "--ed" "Run Gradle with all task actions disabled" "--dry-run")] ["Environment options" (6 "-b" gradlew-transient--build-file) (6 "-c" gradlew-transient--settings-file) (6 "-I" gradlew-transient--init-script) (6 "-p" gradlew-transient--project-dir) (6 "-P" gradlew-transient--project-property) (4 "-D" gradlew-transient--system-property) (5 "-g" gradlew-transient--gradle-home-dir) (4 "--Dja" gradlew-transient--jvm-arguments) (5 "--Djh" gradlew-transient--java-home)] ["Tasks" ("L" "Task list" gradlew-execute-task-from-list) ("O" "Other task" gradlew-transient)] ["Execution" ("RET" "<PLACEHOLDER>" gradlew-transient--execute-task-suffix)] (interactive (list (gradlew--find-script-dir-path) (read-string gradlew//task-prompt-prefix))) (transient-setup 'gradlew-transient nil nil :scope (list gw-path gw-task)))

(autoload 'gradlew-execute-task "gradlew" "\
Execute a Gradlew task (GW-TASK) via the Gradlew script found at GW-PATH.

\(fn GW-PATH GW-TASK)" t nil)

(autoload 'gradlew-execute-task-from-list "gradlew" "\
Execute an available Gradle Wrapper task from all project availables.

This function will parse the output of \"./gradlew tasks --all\" into
`completing-read' candidates to choose from. The parsed candidates are
stored in `gradlew//cached-task-list' so repeated calls can reuse those
quickly.

With a given prefix ARG one can force the reload of the saved task list.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gradlew" '("gradlew")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gradlew-autoloads.el ends here
