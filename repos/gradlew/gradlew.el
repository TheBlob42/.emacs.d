;;; gradlew.el --- gradlew utility tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2020 TheBlob42

;; Author: TheBlob42
;; Version: 0
;; Created 21.10.2020

;; Keywords: java, groovy

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Execute Gradle Wrapper tasks of your projects from within Emacs with ease

;;; Code:

(require 's)
(require 'dash)

(require 'projectile)
(require 'subr-x)
(require 'transient)

(defgroup gradlew nil
  "Gradle wrapper task utilitiy functions."
  :prefix "gradlew-"
  :group 'applications)

;;
;; private variables
;;

(defconst gradlew//task-prompt-prefix "Gradlew Task: ")

(defvar gradlew//cached-task-list '()
  "Property list to cache all parsed Gradle Wrapper tasks per project.")

;;
;; customizations
;;

(defcustom gradlew/script-name "gradlew"
  "The name of the Gradle Wrapper script."
  :group 'gradlew
  :type '(choice
	  (const :tag "Linux & Mac" "gradlew")
	  (const :tag "Windows" "gradlew.bat")))

(defcustom gradlew/post-dispatch-fn nil
  "Function to call after the Gradle Wrapper task has been dispatched."
  :group 'gradlew
  :type 'function)

;;
;; private functions
;;

(defun gradlew--find-script-dir-path ()
  "Get the path of the current projects Gradle Wrapper script.
If there are multiple candidates, present a seletion via `completing-read'."
  (if-let* ((proot (projectile-project-root)))
      ;; search all gradle wrapper scripts inside the current project directory
      (let ((gw-script-paths (-map (-partial 's-replace-regexp gradlew/script-name "")
                                   (directory-files-recursively proot (format "^%s$" gradlew/script-name)))))
	(print gw-script-paths)
	(if (not (zerop (length gw-script-paths)))
	    ;; if there is more than one wrapper script prompt a selection
            (if (> (length gw-script-paths) 1)
		(completing-read "Select a Gradle Wrapper: " gw-script-paths)
              (car gw-script-paths))
	  (error (format "No '%s' script found! Are you inside a \"Gradle Wrapper\" based project?" gradlew/script-name))))
    (error "Not inside a project!")))

;;
;; transient infix arguments
;;

(transient-define-argument gradlew-transient--log-level-argument ()
  :description "Log level"
  :class 'transient-option
  :argument "-Dorg.gradle.logging.level="
  :reader #'(lambda (&rest _) (completing-read "Log level: " (list "quit" "warn" "lifecycle" "info" "debug"))))

(transient-define-argument gradlew-transient--console-output-type ()
  :description "Console output type"
  :class 'transient-option
  :argument "--console="
  :reader #'(lambda (&rest _) (completing-read "Console output type: " (list "auto" "plain" "rich" "verbose"))))

(transient-define-argument gradlew-transient--warning-mode ()
  :description "Warning mode"
  :class 'transient-option
  :argument "--warning-mode="
  :reader #'(lambda (&rest _) (completing-read "Warning mode: " (list "all" "fail" "none" "summary"))))

(transient-define-argument gradlew-transient--build-file ()
  :description "Specify the build file"
  :class 'transient-option
  :argument "--build-file="
  :reader #'(lambda (&rest _) (read-file-name "Build file: ")))

(transient-define-argument gradlew-transient--settings-file ()
  :description "Specify Gradle settings file"
  :class 'transient-option
  :argument "--settings-file="
  :reader #'(lambda (&rest _) (read-file-name "Settings file: ")))

(transient-define-argument gradlew-transient--gradle-home-dir ()
  :description "Specify Gradle user home directory"
  :class 'transient-option
  :argument "--gradle-user-home="
  :reader #'(lambda (&rest _) (read-directory-name "Gradle user home: ")))

(transient-define-argument gradlew-transient--project-dir ()
  :description "Specify the project directory"
  :class 'transient-option
  :argument "--project-dir="
  :reader #'(lambda (&rest _) (read-directory-name "Project directory: ")))

(transient-define-argument gradlew-transient--system-property ()
  :description "Set a system property of the JVM"
  :class 'transient-option
  :argument "-D"
  :reader #'(lambda (&rest _)
	      (let ((property (read-string "System property: "))
		    (value (read-string "Value: ")))
		(concat property "=" value))))

(transient-define-argument gradlew-transient--init-script ()
  :description "Initialization script"
  :class 'transient-option
  :argument "--init-script="
  :reader #'(lambda (&rest _) (read-file-name "Init script: ")))

(transient-define-argument gradlew-transient--project-property ()
  :description "Set project property of the root project"
  :class 'transient-option
  :argument "-P"
  :reader #'(lambda (&rest _)
	      (let ((property (read-string "Project property: "))
		    (value (read-string "Value: ")))
		(concat property "=" value))))

(transient-define-argument gradlew-transient--jvm-arguments ()
  :description "Set JVM arguments"
  :class 'transient-option
  :argument "-Dorg.gradle.jvmargs="
  :reader #'(lambda (&rest _) (read-string "JVM arguments: ")))

(transient-define-argument gradlew-transient--java-home ()
  :description "Set JDK home directory"
  :class 'transient-option
  :argument "-Dorg.gradle.java.home="
  :reader #'(lambda (&rest _) (read-directory-name "JDK home directory: ")))

;;
;; transient suffixes
;;

(transient-define-suffix gradlew-transient--execute-task-suffix (&optional args)
  "Transient suffix for `gradlew-transient' which executes the gradlew task."
  (interactive (list (transient-args 'gradlew-transient)))
  (let* ((scope (oref transient-current-prefix scope))
	 (gw-path (car scope))
	 (gw-task (car (cdr scope)))
	 (gw-arg-string (when args (concat " " (s-join " " args)))))
    (gradlew-execute-task gw-path (concat gw-task gw-arg-string))))

;;
;; gradlew transient
;;

;;;###autoload
(transient-define-prefix gradlew-transient (gw-path gw-task)
  "The main entry point to configure and execute any Gradlew task."
  ["Debugging Options"
   (5 "-v" "Print version information" "--version")
   (3 "-s" "Print ou stacktrace" "--stacktrace")
   (3 "-S" "Print out FULL stacktrace" "--full-stacktrace")
   (6 "--Dg" "Debug Gradle client" "-Dorg.gradle.debug=true")
   (6 "--Dd" "Debug Gradle Daemon" "-Dorg.gradle.daemon.debug=true")
   (3 "--d" "Debug JVM" "--debug-jvm")]
  ["Logging options"
   (4 "-q" "Log errors only" "--quit")
   (4 "-w" "Log level warn" "--warn")
   (4 "-i" "Log level info" "--info")
   (4 "-d" "Log in debug mode" "--debug")
   (7 "--ll" gradlew-transient--log-level-argument)
   (7 "--lc" gradlew-transient--console-output-type)
   (7 "--lw" gradlew-transient--warning-mode)]
  ["Daemon options"
   (8 "--dd" "Use Gradle Daemon (default)" "--daemon")
   (8 "--dn" "Do not use the Gradle Daemon" "--no-daemon")
   (8 "--df" "Start Daemon in a foreground process" "--foreground")]
  ["Execution options"
   (5 "--eo" "Operating without network access" "--offline")
   (3 "--er" "Refresh the state of dependencies" "--refresh-dependencies")
   (6 "--ed" "Run Gradle with all task actions disabled" "--dry-run")]
  ["Environment options"
   (6 "-b" gradlew-transient--build-file)
   (6 "-c" gradlew-transient--settings-file)
   (6 "-I" gradlew-transient--init-script)
   (6 "-p" gradlew-transient--project-dir)
   (6 "-P" gradlew-transient--project-property)
   (4 "-D" gradlew-transient--system-property)
   (5 "-g" gradlew-transient--gradle-home-dir)
   (4 "--Dja" gradlew-transient--jvm-arguments)
   (5 "--Djh" gradlew-transient--java-home)]
  ["Tasks"
   ("L" "Task list" gradlew-execute-task-from-list)
   ("O" "Other task" gradlew-transient)]
  ["Execution"
   ;; the description here is just a placeholder and will be
   ;; replaced with every call by `gradlew-transient--execute-task-suffix-advice'
   ("RET" "<PLACEHOLDER>" gradlew-transient--execute-task-suffix)]
  (interactive (list (gradlew--find-script-dir-path)
		     (read-string gradlew//task-prompt-prefix)))
  (transient-setup 'gradlew-transient nil nil :scope (list gw-path gw-task)))

;; to always show the current task name as a descriptions in the transient menu
;; we have to advice the function and update it dynamically on every call
(defun gradlew-transient--advice (_gw-path gw-task)
  "Update the execution suffix description dynamically.

Uses `transient-suffix-put' in order to set the description of
`gradlew-transient--execute-task-suffix' based on the GW-TASK name."
  (transient-suffix-put 'gradlew-transient "RET" :description
			(concat "Execute " (propertize gw-task 'face 'font-lock-keyword-face))))
(advice-add 'gradlew-transient :before 'gradlew-transient--advice)

;;
;; interactive functions
;;

;;;###autoload
(defun gradlew-execute-task (gw-path gw-task)
  "Execute a Gradlew task (GW-TASK) via the Gradlew script found at GW-PATH."
  (interactive (list (gradlew--find-script-dir-path)
		     (read-string gradlew//task-prompt-prefix)))
  (let ((command (format "cd %s && ./%s %s\n" gw-path gradlew/script-name gw-task))
	(shell-buffer-name (format "*gw:%s:%s*" (projectile-project-name) gw-task)))
    (comint-send-string (get-buffer-process (shell shell-buffer-name)) command)
    ;; moves to the end of the buffer in order to see the most recent logs
    (goto-char (point-max))
    ;; call post-dispatch-fn if present
    (when gradlew/post-dispatch-fn
      (funcall gradlew/post-dispatch-fn))))

;;;###autoload
(defun gradlew-execute-task-from-list (&optional arg)
  "Execute an available Gradle Wrapper task from all project availables.

This function will parse the output of \"./gradlew tasks --all\" into
`completing-read' candidates to choose from. The parsed candidates are
stored in `gradlew//cached-task-list' so repeated calls can reuse those
quickly.

With a given prefix ARG one can force the reload of the saved task list."
  (interactive "P")
  (let ((gw-path (gradlew--find-script-dir-path))
	(current-project-symbol (intern (projectile-project-root))))
    (when (or arg                                                  ; prefix arg provided
	      (not (plist-member gradlew//cached-task-list  ; no tasks saved yet
				 current-project-symbol)))
      ;; (re)load and parse the Gradle Wrapper task list
      (let* ((gradlew-task-output (shell-command-to-string (format "cd %s && ./%s tasks --all" gw-path gradlew/script-name)))
	     (gradlew-error (-second-item (s-match "What went wrong:\n\\(.*\\)\n" gradlew-task-output))))
	(if (not gradlew-error)
	    (let* ((gradlew-task-group-strings (s-match-strings-all
						"\\(^[a-zA-Z ]+\\)\n-+\n\\(\\(?:[a-zA-Z0-9-_]+?\\(?: - .*?\\)?.*\n\\)+\\)"
						gradlew-task-output))
		   (task-items (-flatten
				(-map (lambda (x)
					(let ((group-name (-second-item x))                           ; extract the task group name
					      (group-tasks (-filter 's-present?                       ; filter out empty strings
								    (s-split "\n" (-third-item x))))) ; split into separate lines
					  ;; build formatted task strings -> "[group name] task - task description"
					  (-map (-partial 'concat "[" group-name "] ") group-tasks)))
				      gradlew-task-group-strings))))
	      ;; save parsed `task-items' for reusage
	      (setq gradlew//cached-task-list (plist-put gradlew//cached-task-list
							 current-project-symbol
							 task-items)))
	  (error (concat "The Gradle Wrapper script threw an error: " gradlew-error)))))
    (let ((gw-task (-second-item
		    (s-match "^\\[.*?\\] \\([^ ]*\\)\\(?: - .*\\)?"
			     (completing-read "Select task: " (plist-get gradlew//cached-task-list
									 current-project-symbol))))))
      ;; execute the selected Gradle Wrapper task
      (gradlew-transient gw-path gw-task))))

(provide 'gradlew)

;;; gradlew.el ends here
