;;; gradlew-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gradlew" "gradlew.el" (0 0 0 0))
;;; Generated autoloads from gradlew.el

(autoload 'gradlew-execute-task "gradlew" "\
Execute a Gradlew task (GW-TASK) via the Gradlew script found at GW-PATH.
With a prefix argument this calls the `gradlew-transient' afterwards for additional configuration.

\(fn GW-PATH GW-TASK)" t nil)

(autoload 'gradlew-execute-task-from-list "gradlew" "\
Execute an available Gradle Wrapper task from all project availables.

This function will parse the output of \"./gradlew tasks --all\" into
`completing-read' candidates to choose from. The parsed candidates are
stored in `gradlew//cached-task-list' so repeated calls can reuse those
quickly.

With a numeric prefix ARG one can force the reload of the saved task list.

With any prefix ARG, call the `gradlew-transient' after the selection for
further task configuration and parameters.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "gradlew" '("gradlew"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gradlew-autoloads.el ends here
