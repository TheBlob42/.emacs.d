;;; init.el --- My personal emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal Emacs config
;; Inspired by Spacemacs and Doom Emacs.

;;; Conventions:

;;;; Functions
;; 'my/xxx' is an interactive function
;; 'my//xxx' is a private function

;;;; Variables
;; 'my-xxx' is a variable
;; 'my--xxx' is a private variable

;;; Code:

;;;* startup optimizations

;; These tricks and techniques are mostly borrowed from the excellent doom-emacs FAQ
;; (https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly)

;; save 'file-name-handler' reference for after startup reset
(defvar my--file-name-handler-alist file-name-handler-alist)

(setq ;; unset 'file-name-handler-alist' temporarily
      file-name-handler-alist nil
      ;; turning up garbage collection threshold and percentage temporarily
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; increase the amount of data emacs reads from processes
      ;; [src: https://github.com/emacs-lsp/lsp-mode#performance]
      read-process-output-max (* 3 1024 1024))

(add-hook 'emacs-startup-hook
  (lambda ()
    ;; print the startup time once after the startup
    (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
    (setq
     ;; reset garbage collection values after startup to avoid freezes due to gc
     gc-cons-threshold 16777216
     gc-cons-percentage 0.1
     ;; reset 'file-name-handler-alist' to avoid complications
     file-name-handler-alist my--file-name-handler-alist)))

;;;* basic settings

;;;** sane defaults

;; answering with 'y' or 'n' is sufficient
(defalias 'yes-or-no-p 'y-or-n-p)

(setq
 inhibit-startup-screen t                    ; disable start-up screen
 initial-scratch-message ""                  ; empty the initial *scratch* buffer
 initial-major-mode 'text-mode               ; set scratch buffer major mode
 sentence-end-double-space nil               ; end sentences with just one space
 create-lockfiles nil                        ; lockfiles don't provide a lot of benefit
 scroll-conservatively most-positive-fixnum  ; always scroll by one line
 ring-bell-function 'ignore                  ; turn off the bell sound
 x-stretch-cursor t                          ; make cursor the width of the character underneath (i.e. full width of a TAB)
 delete-by-moving-to-trash t                 ; move deleted files to trash instead of deleting them outright
 help-window-select t)                       ; focus new help windows when opened

(set-default-coding-systems 'utf-8)         ; default to utf-8 encoding
(add-to-list 'default-frame-alist
	     '(fullscreen . maximized))     ; maximize the emacs window on startup
(global-hl-line-mode)                       ; highlight current line

;; remove not needed GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(tooltip-mode -1)

;; activate 'winner-mode' to enable window layout undo/redo
(winner-mode)

;; write the customizations block to another file, but never load it
(setq custom-file (concat user-emacs-directory "ignore-customizations.el"))

;; |--------------------------------------------------|
;; |--- kill ring adaptions

;; replace kill-region with delete-region (don't write to kill ring)
;; main reason is to prevent 'M-backspace' from writing to the kill ring
(advice-add 'kill-region :override 'delete-region)

;;;** backups

;; Improve the default configuration for file backups

(defvar my-backup-directory (concat user-emacs-directory "backups"))
;; create the backup folder if it does not exist yet
(when (not (file-exists-p my-backup-directory))
  (make-directory my-backup-directory t))

(setq
 ;; save all backup files to a backup folder inside the emacs directory
 backup-directory-alist `(("." . ,my-backup-directory))
 make-backup-files t   ; backup a file the first time it is saved
 backup-by-copying t   ; don't get problems with symlinks
 version-control t     ; version numbers for backup files
 delete-old-versions t ; delete excess backup files silently
 kept-old-versions 6   ; oldest version to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9   ; newest version to keep when a new numbered backup is made (default: 2)
 auto-save-default t)  ; auto-save every buffer that visits a file

;;;** fonts

;; use 'Source Code Pro' as emacs default font
(set-face-attribute 'default nil :font "Source Code Pro Medium" :height 110)

;; to make sure that everything uses your desired font family we configure the fixed-pitch (monospaced) and variable-pitch (proportional spacing) faces
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro Medium" :height 110)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro Medium" :height 110)

;; set the fall back font [src: https://idiocy.org/emacs-fonts-and-fontsets.html]
(set-fontset-font t 'latin "Noto Sans")

;;;** use-package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; to prevent initializing twice
(setq package-enable-at-startup nil)
;; don't add that 'custom-set-variables' block to the init.el
(advice-add #'package--ensure-init-file :override #'ignore)

(unless (bound-and-true-p package--initialized)
   (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq
 use-package-always-ensure t       ; ensure all packages added via 'use-package'
 use-package-compute-statistics t) ; enable this to see package loading statistics

;;;** some basic requirements

;; to ensure everything is working fine you have to install the fonts
;; necessary by running the command "M-x all-the-icons-install-fonts"
(use-package all-the-icons)

;; utility libraries to make emacs lisp a more viable programming language
(use-package s)
(use-package dash)
(use-package dash-functional)

;;;** user config

;; load configuration variables for different devices from a specific config file
(defvar my-config-file (concat user-emacs-directory "config"))

(defun my//get-value-from-config (key &optional default)
  "Return the value specified in the user config file for the given KEY.
If DEFAULT is passed it will be evaled and returned in the case of an error (for example no config file present, key not specified, etc.)"
  (if (not (file-exists-p my-config-file))
    (if (not (null default)) (eval default)
      (error (concat "The specified user config file does not exists: '" my-config-file "'. Please check the value of 'my-config-file'!")))
    (with-temp-buffer
      (insert-file-contents my-config-file)
      (let ((value (-second-item (s-match (concat key ": ?\\(.*\\)\n?$")
					  (buffer-string)))))
	(if (not value)
	  (if (not (null default)) (eval default)
	    (error (concat "User config does not contain any value for the key: '" key "'")))
	  value)))))

;;;* styling and appeal

;;;** theme

;; light theme
(use-package modus-operandi-theme
  :custom
  (modus-operandi-theme-rainbow-headings t)
  (modus-operandi-theme-distinct-org-blocks t)
  :config
  (defun my/load-modus-operandi-theme ()
    "Load the 'modus-operandi' theme with some slight modifications."
    (load-theme 'modus-operandi t)
    ;; change term white to gray to make it more readable on the light background
    (with-eval-after-load "term"
      (set-face-attribute 'term-color-white nil :foreground "dark gray"))))

;; dark theme
(use-package modus-vivendi-theme
  :custom
  (modus-vivendi-theme-rainbow-headings t)
  (modus-vivendi-theme-distinct-org-blocks t)
  :config
  (defun my/load-modus-vivendi-theme ()
    "Load the 'modus-vivendi' theme with some slight modifications."
    (load-theme 'modus-vivendi t)
    ;; we have to manually reset the 'hl-line' color to its origin
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "#151823"))
    ;; reset the color change from 'modus-operandi-theme' for 'term-color-white'
    (with-eval-after-load "term"
      (set-face-attribute 'term-color-white nil :foreground "white")))

  (defvar my--dark-mode-enabled nil
    "State indicator if the dark mode theme is currently enabled")

  (defun my/toggle-dark-mode ()
    "Switches the theme from light to dark and vice versa."
    (interactive)
    (if my--dark-mode-enabled
      (my/load-modus-operandi-theme)
      (my/load-modus-vivendi-theme))
    (setq my--dark-mode-enabled (not my--dark-mode-enabled)))

  (defun my//dark-mode-which-key-replacement (entry)
    "Which key replacement function that shows the currently present state."
    (let ((key (car entry)))
      (if my--dark-mode-enabled
	`(,key . "[X] dark mode")
	`(,key . "[ ] dark mode"))))

  (defun my//load-theme-on-startup ()
    "Checks the current time and loads the appropriate theme (light or dark) for it."
    (let ((hour (string-to-number
			 (substring (current-time-string) 11 13))))
      (if (member hour (number-sequence 7 19))
	(my/load-modus-operandi-theme)
	(progn
	  (setq my--dark-mode-enabled t)
	  (my/load-modus-vivendi-theme)))))

  ;; load startup theme depending on the current time
  (my//load-theme-on-startup))

;;;** modeline

(setq-default mode-line-format
              (list
	       " "
	       ;; display the 'winum' window number
	       '(:eval (propertize (winum-get-number-string (selected-window))
				   'face 'font-lock-keyword-face))
	       " "
	       " "
	       ;; display the buffer name
	       ;; for "non-star-buffers" show the modification status by changing the font color and add an asterisk icon
	       '(:eval (let ((is-star-buffer? (s-matches? "^\s*\\*" (buffer-name))))
			 (concat
			  (when (and (not is-star-buffer?)
				     (buffer-modified-p))
			    (concat
			     (all-the-icons-faicon "asterisk" :face 'all-the-icons-orange :v-adjust -0.1)
			     " "))
			  
			  (propertize "%b" 'face
				      ;; never mark star-buffers as modified
				      (if is-star-buffer?
					'font-lock-string-face
					(if (buffer-modified-p)
					  'font-lock-warning-face
					  'font-lock-string-face))))))
	       " "
	       ;; display cursor position in document (in % from top)
	       (propertize "%p" 'face 'font-lock-constant-face)
	       ;; display the flycheck error/warning count (if there are any)
	       '(:eval (my//flycheck-status))
	       ;; display the current LSP status
	       '(:eval (when (bound-and-true-p lsp-mode)
			 (concat
			  " "
			  (if (lsp-workspaces)
			    (all-the-icons-faicon "rocket" :face 'all-the-icons-green :v-adjust -0.07)
			    (all-the-icons-faicon "rocket" :face 'all-the-icons-red :v-adjust -0.07))
			  (lsp-mode-line))))
	       ;; display the current debug state (dap-mode)
	       '(:eval (when (and (bound-and-true-p lsp-mode)
				  (bound-and-true-p dap-mode))
			 (when-let ((session (dap--cur-session)))
			   (when (dap--session-running session)
			     (concat
			      " "
			      (all-the-icons-faicon "bug" :face 'all-the-icons-purple-alt :v-adjust -0.1))))))
	       ;; --> align the following to the right
	       '(:eval (let ((info-string
			      (concat
			       ;; show the current VC branch
			       (when vc-mode
				 (concat
				  " "
				  ;; use 'substring' to strip the "Git: " prefix from the branch name
				  (substring vc-mode 5)))
			       ;; for "non-star-buffer" show the current buffer size
			       (when (not (s-matches? "^\s*\\*" (buffer-name)))
				 (concat
				  " "
				  (propertize "%I" 'face 'font-lock-constant-face)))
			       ;; show the appropriate major-mode icon
			       " "
			       (cond
				((equal 'java-mode major-mode) "Java")
				((equal 'wdired-mode major-mode) (all-the-icons-faicon "pencil" :v-adjust -0.1))
				((-contains? '(js-mode text-mode) major-mode) (all-the-icons-icon-for-mode major-mode :v-adjust 0))
				(t (all-the-icons-icon-for-mode major-mode :v-adjust -0.1)))
			       "  ")))
			 (concat
			  (propertize
			   " " 'display
			   `((space :align-to (- (+ right right-fringe right-margin)
						 ,(+ 3 (string-width info-string))))))
			  info-string)))))

(defun my//flycheck-status ()
  "If flycheck-mode is enabled, check for the current status and show an appropriate icon plus the number of warnings/errors (if any are present)."
  (when (bound-and-true-p flycheck-mode)
    ;; declare variables and functions to prevent compiler warnings
    (defvar flycheck-last-status-change)
    (defvar flycheck-current-errors)
    (declare-function flycheck-count-errors "flycheck" t)
    (declare-function all-the-icons-faicon "all-the-icons" t)
    (concat
     " "
     (pcase flycheck-last-status-change
       (`finished (let* ((all-errors (flycheck-count-errors flycheck-current-errors))
			 (warnings
			  ;; check for entries that contain the keyword "warning", then sum up all the ocurrences
			  ;; (e.g. "lsp-flycheck-warning-unnecessary", "lsp-flycheck-warning-deprecated")
			  (-reduce-from '+ 0 (-map 'cdr (-filter (-compose (-partial 's-contains? "warning")
									   'symbol-name
									   '-first-item) all-errors))))
			 (errors (assq 'error all-errors)))
		    (cond
		     (errors (concat
			      (all-the-icons-faicon "ban" :face '(:foreground "red") :v-adjust -0.05)
			      (propertize (format ":%s" (cdr errors))
					  'face '(:foreground "red" :weight bold))))
		     ((> warnings 0) (concat
				      (all-the-icons-faicon "exclamation-circle" :face '(:foreground "dark orange") :v-adjust -0.05)
				(propertize (format ":%s" warnings)
					    'face '(:foreground "dark orange" :weight bold))))
		     (t (all-the-icons-faicon "check-circle" :face '(:foreground "dark green") :v-adjust -0.05)))))
       (`running (all-the-icons-faicon "spinner" :face 'all-the-icons-blue :v-adjust -0.1))
       (`no-checker "")
       (`not-checked (all-the-icons-faicon "frown-o" :v-adjust -0.1))
       (`errored (all-the-icons-faicon "exclamation" :v-adjust -0.1))
       (`interrupted (all-the-icons-faicon "plug" :v-adjust -0.1))
       (`suspicious (all-the-icons-faicon "bug" :v-adjust -0.1))))))

;;;* keybindings

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config (which-key-mode 1))

(use-package general
  :init
  ;; define all first level infixes
  (defvar my/infix/frames "F")
  (defvar my/infix/toggle "T")
  (defvar my/infix/buffer "b")
  (defvar my/infix/dired "D")
  (defvar my/infix/custom "c")
  (defvar my/infix/errors "e")
  (defvar my/infix/files "f")
  (defvar my/infix/git "g")
  (defvar my/infix/insert "i")
  (defvar my/infix/jump "j")
  (defvar my/infix/projects "p")
  (defvar my/infix/quit "q")
  (defvar my/infix/search "s")
  (defvar my/infix/tabs "t")
  (defvar my/infix/windows "w")
  :config
  ;; |--------------------------------------------------|
  ;; |##################################################|
  ;; |### Create definers for the leader key
  
  (general-create-definer my/leader-key
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer my/major-mode-leader-key
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  ;; unbind "SPC" to prevent conflicts with other keybindings
  (my/leader-key "" nil)

  ;; |--------------------------------------------------|
  ;; |##################################################|
  ;; |### Create definers for different evil states
  
  (general-create-definer my/all-states-keys
    :states '(normal visual motion emacs insert))
  (general-create-definer my/normal-state-keys
    :states '(normal motion))
  (general-create-definer my/visual-state-keys
    :states '(visual))
  (general-create-definer my/insert-state-keys
    :states '(insert))
  
  ;; |--------------------------------------------------|
  ;; |--- General emacs keybindings

  (my/normal-state-keys
    "C-M-<backspace>" 'delete-indentation)
  (my/insert-state-keys
    "C-d" 'delete-char
    "C-M-<backspace>" 'delete-indentation)
  
  ;; |--------------------------------------------------|
  ;; |--- Frames
  
  (defun my/new-frame ()
    "Create a new frame and focus it."
    (interactive)
    (select-frame (make-frame)))
  
  (my/leader-key
    :infix my/infix/frames
    "" '(:ignore t :which-key "Frames")
    "n" '(my/new-frame :which-key "new frame")
    "o" '(other-frame :which-key "switch frame")
    "d" '(delete-frame :which-key "delete frame"))
  
  ;; |--------------------------------------------------|
  ;; |--- Toggle & Show
  
  (my/leader-key
    :infix my/infix/toggle
    "" '(:ignore t :which-key "Toggles")
    "d" '(my/toggle-dark-mode :which-key my//dark-mode-which-key-replacement)
    "w" '(whitespace-mode :which-key "whitespaces")
    "t" '(toggle-truncate-lines :which-key "truncated lines"))

  ;; |--------------------------------------------------|
  ;; |--- Buffers

  (defun my/switch-to-last-buffer ()
    "Switch to the last buffer that was visible in the current window."
    (interactive)
    (switch-to-buffer nil))

  (my/leader-key "TAB" '(my/switch-to-last-buffer :which-key "previous buffer"))

  (defun my/new-empty-buffer ()
    "Create a new empty buffer."
    (interactive)
    (let ((buffer (generate-new-buffer "untitled")))
      (switch-to-buffer buffer)
      ;; use text-mode as the default major-mode
      (text-mode)
      ;; disable font-lock-mode in case you want to insert large amounts of text
      (font-lock-mode -1)
      ;; mark buffer as modified, so we are able to save it as an empty file
      (set-buffer-modified-p t)
      ;; ask if the buffer should be saved when quitting emacs
      (setq buffer-offer-save t)
      buffer))
  
  ;; source: https://emacs.stackexchange.com/a/171
  (defun my/revert-buffer ()
    "Synchronize the buffer state with the corresponding file."
    (interactive)
    (revert-buffer t (not (buffer-modified-p)) t))

  (my/leader-key
    :infix my/infix/buffer
    "" '(:ignore t :which-key "Buffers")
    "a" '(mark-whole-buffer :which-key "select all content")
    "n" '(my/new-empty-buffer :which-key "new")
    "u" '(undo-tree-visualize :which-key "undo-tree")
    "d" '(kill-current-buffer :which-key "kill")
    "r" '(my/revert-buffer :which-key "revert"))

  ;; |--------------------------------------------------|
  ;; |--- Errors

  (my/leader-key
    :infix my/infix/errors
    "" '(:ignore t :which-key "Errors"))

  ;; |--------------------------------------------------|
  ;; |--- Files

  (defun my/save-buffer-to-file ()
    "Save the buffer content to a new file."
    (interactive)
    (write-region (point-min)
		  (point-max)
		  (read-file-name "New filename: " buffer-file-name nil nil nil)))
  
  (defun my/delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when (and filename (y-or-n-p (concat "Do you really want to delete '" buffer-file-name "'")))
	(if (vc-backend filename)
          (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer))))))
  
  (defun my/rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
	(let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

  (my/leader-key
    :infix my/infix/files
    "" '(:ignore t :which-key "Files")
    "s" '(save-buffer :which-key "save file")
    "e" '(:ignore t :which-key "Edit")
    "er" '(my/rename-file-and-buffer :which-key "rename")
    "ed" '(my/delete-file-and-buffer :which-key "delete")
    "ec" '(my/save-buffer-to-file :which-key "copy"))

  ;; |--------------------------------------------------|
  ;; |--- Git

  (my/leader-key
    :infix my/infix/git
    "" '(:ignore t :which-key "Git"))

  ;; |--------------------------------------------------|
  ;; |--- Insert

  (defun my/insert-relative-file-path (filename)
    "Insert the relative path to FILENAME (from the current buffer/file)."
    (interactive "*fInsert file name: \n")
    (insert (file-relative-name filename)))
  
  (defun my/insert-full-file-path (filename)
    "Insert the absolute path to FILENAME."
    (interactive "*fInsert file name: \n")
    (insert filename))

  (my/leader-key
    :infix my/infix/insert
    "" '(:ignore t :which-key "Insert")
    "f" '(my/insert-full-file-path :which-key "file path")
    "F" '(my/insert-relative-file-path :which-key "relative file path"))

  ;; |--------------------------------------------------|
  ;; |--- Jump

  (my/leader-key
    :infix my/infix/jump
    "" '(:ignore t :which-key "Jump"))

  ;; |--------------------------------------------------|
  ;; |--- Projects

  (my/leader-key
    :infix my/infix/projects
    "" '(:ignore t :which-key "Projects"))

  ;; |--------------------------------------------------|
  ;; |--- Dired

  (my/leader-key
    :infix my/infix/dired
    "" '(:ignore t :which-key "Dired"))

  ;; |--------------------------------------------------|
  ;; |--- Quit

  (my/leader-key
    :infix my/infix/quit
    "" '(:ignore t :which-key "Quit")
    "q" '(save-buffers-kill-terminal :which-key "quit"))

  ;; |--------------------------------------------------|
  ;; |--- Search

  (my/leader-key
    :infix my/infix/search
    "" '(:ignore t :which-key "Search"))

  ;; |--------------------------------------------------|
  ;; |--- Tabs

  (my/leader-key
    :infix my/infix/tabs
    "" '(:ignore t :which-key "Tabs"))

  ;; |--------------------------------------------------|
  ;; |--- Window

  (my/leader-key
   :infix my/infix/windows
   "" '(:ignore t :which-key "Windows")
   "m" '(delete-other-windows :which-key "maximize")
   "u" '(winner-undo :which-key "winner undo")
   "x" '(kill-buffer-and-window :which-key "kill buffer & window"))
  
  ;; |--------------------------------------------------|
  ;; |--- Custom

  (my/leader-key
    :infix my/infix/custom
    "" '(:ignore t :which-key "Custom")))

(use-package hydra
  :general
  (my/leader-key
    :infix my/infix/buffer
    "+" '(hydra-zoom/body :which-key "~Text Size~"))
  (my/leader-key
    :infix my/infix/windows
    "+" '(hydra-window/body :which-key "~Window Size~"))
  :config
  (defun my/default-text-size ()
    "Reset the text size to the default value."
    (interactive)
    (text-scale-set 0))

  (defun my/shrink-window-vertically ()
    "Shrink the vertical window size."
    (interactive)
    (let ((current-prefix-arg '(-1)))
      (call-interactively 'enlarge-window)))
  
  ;; hydra to zoom the text inside the current buffer
  (defhydra hydra-zoom ()
    "Text zoom level (current buffer)"
    ("+" text-scale-increase "zoom in")
    ("-" text-scale-decrease "zoom out")
    ("0" my/default-text-size "default")
    ("q" nil "quit"))

  ;; hydra to size the current window
  (defhydra hydra-window (:hint nil)
    "
^Vertical^    ^Horizontal^  ^Balance^
^^^^^^^^---------------------------------
_v_: enlarge  _h_: enlarge  _=_: balance
_V_: shrink   _H_: shrink
^^^^^^^^---------------------------------
[_q_]: quit
^^^^^^^^---------------------------------
"
    ("v" enlarge-window)
    ("V" my/shrink-window-vertically)
    ("h" enlarge-window-horizontally)
    ("H" shrink-window-horizontally)
    ("=" balance-windows)
    ("q" nil)))

;;;* evil

;; save the startup cursor color of the current theme for later usage
(defvar my--startup-cursor-color (face-attribute 'cursor :background))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-kill-on-visual-paste nil)
  ;; style 'evil-emacs-state-cursor' to differentiate itself from the other states
  (evil-emacs-state-cursor '("purple" (bar . 3)))
  (evil-normal-state-cursor   `(,my--startup-cursor-color box))
  (evil-visual-state-cursor   `(,my--startup-cursor-color box))
  (evil-insert-state-cursor   `(,my--startup-cursor-color (bar . 2)))
  (evil-operator-state-cursor `(,my--startup-cursor-color evil-half-cursor))
  (evil-motion-state-cursor   `(,my--startup-cursor-color box))
  (evil-replace-state-cursor  `(,my--startup-cursor-color hbar))
  ;; these options are needed by 'evil-collection'
  (evil-want-integration t)
  (evil-want-keybinding nil)
  ;; window split options
  (evil-split-window-below nil)
  (evil-vsplit-window-right nil)
  :config
  ;; |--------------------------------------------------|
  ;; |--- kill ring adaptions
  
  ;; prevent 'evil-delete from using the kill ring and use the black hole register instead
  ;; [src: https://github.com/syl20bnr/spacemacs/issues/6977#issuecomment-244014379]
  (defun my//evil-delete (orig-fn beg end &optional type _ &rest args)
    "Call 'evil-delete' with the optional 'black hole register' (?_) argument.
ORIG-FN is 'evil-delete'. BEG, END, TYPE and ARGS are just passed through."
    (apply orig-fn beg end type ?_ args))

  (advice-add 'evil-delete :around 'my//evil-delete)

  ;; use "x" for cutting (copy then delete) in visual state
  ;; instead of being just another shortcut for delete
  (defun my/cut ()
    "Copy the selected region, then delete it."
    (interactive)
    (evil-yank (region-beginning) (region-end))
    (evil-delete-char (region-beginning) (region-end))
    (evil-force-normal-state))

  (my/visual-state-keys "x" 'my/cut)

  ;; |--------------------------------------------------|
  ;; |--- unbind mouse events
  
  (general-unbind
    ;; left click
    "<mouse-1>"
    "<down-mouse-1>"
    "<drag-mouse-1>"
    "<S-down-mouse-1>"
    "<M-down-mouse-1>"
    "<M-mouse-1>"
    "<C-mouse-1>"
    "<C-down-mouse-1>"
    "<M-down-mouse-1>"
    "<C-M-mouse-1>"
    "<C-M-down-mouse-1>"
    ;; right click
    "<mouse-3>"
    "<down-mouse-3>"
    "<C-down-mouse-3>"
    "<S-mouse-3>"
    "<S-down-mouse-3>"
    "<M-mouse-3>")

  (general-unbind 'motion "<down-mouse-1>")
  
  ;; |--------------------------------------------------|
  ;; |--- emacs "style" keybindings in insert state

  (my/insert-state-keys
    "C-a" 'evil-beginning-of-line
    "C-e" 'evil-end-of-visual-line
    "C-S-n" 'evil-next-line
    "C-S-p" 'evil-previous-line)

  ;; |--------------------------------------------------|
  ;; |--- utility insert state bindings

  (my/insert-state-keys
    "M-o" 'evil-open-below
    "M-O" 'evil-open-above
    ;; alternative to 'evil-escape'
    ;; especially useful for the usage with evil-mc
    "C-g" 'evil-normal-state)

  ;; |--------------------------------------------------|
  ;; |--- Toggles
  
  ;; toggle the visual line mode
  (defvar my--visual-line-toggle nil)

  (defun my/toggle-visual-line ()
    "Toggle 'visual-line-mode' and set custom keybindings for it."
    (interactive)
    (if my--visual-line-toggle
      ;; deactivate "visual-line-mode"
      (progn
	(visual-line-mode -1)
	(evil-normalize-keymaps))
      ;; activate "visual-line-mode"
      (progn
	(visual-line-mode)
	(general-define-key
	 :definer 'minor-mode
	 :states '(normal motion)
	 :keymaps 'visual-line-mode
	 "j" 'evil-next-visual-line
	 "k" 'evil-previous-visual-line
	 "<down>" 'evil-next-visual-line
	 "<up>" 'evil-previous-visual-line)
	(evil-normalize-keymaps)))
    (setq-local my--visual-line-toggle (not my--visual-line-toggle)))

  (my/leader-key
    :infix my/infix/toggle
    "l" '(my/toggle-visual-line :which-key "visual lines"))

  ;; |--------------------------------------------------|
  ;; |--- Insert
  
  (defun spacemacs/evil-insert-line-above (count)
    "Insert one or several lines (COUNT) above the current point's line.
It does so without changing the current state and point position."
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
  
  (defun spacemacs/evil-insert-line-below (count)
    "Insert one or several lines (COUNT) below the current point's line.
It does so without changing the current state and point position."
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-below))))
  
  (my/leader-key
    :infix my/infix/insert
    "j" '(spacemacs/evil-insert-line-below :which-key "line below")
    "k" '(spacemacs/evil-insert-line-above :which-key "line above"))
  
  ;; |--------------------------------------------------|
  ;; |--- Windows

  (defun my/evil-vsplit-right-and-focus ()
    "V-Split the current window and focus the new window on the right."
    (interactive)
    (let ((evil-vsplit-window-right t))
      (evil-window-vsplit)))

  (defun my/evil-split-below-and-focus ()
    "Split the current window and focus the new window below."
    (interactive)
    (let ((evil-split-window-below t))
      (evil-window-split)))
  
  (my/leader-key
    :infix my/infix/windows
    "V" '(my/evil-vsplit-right-and-focus :which-key "vsplit and focus")
    "S" '(my/evil-split-below-and-focus :which-key "split and focus")
    "v" '(evil-window-vsplit :which-key "vsplit")
    "s" '(evil-window-split :which-key "split")
    "h" '(evil-window-left :which-key "go left")
    "l" '(evil-window-right :which-key "go right")
    "j" '(evil-window-down :which-key "go down")
    "k" '(evil-window-up :which-key "go up")
    "d" '(:ignore t :which-key "Delete")
    "dd" '(evil-window-delete :which-key "current"))
  ;; |--------------------------------------------------|

  (evil-mode))

(use-package evil-collection
  :after evil
  :custom
  ;; deactivate 'company-tng'
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init '(company
			  comint
			  compile
			  dired
			  (package-menu package)
			  (term term ansi-term multi-term)))
  (my/visual-state-keys
    :keymaps 'comint-mode-map
    ;; enable 'evil-scroll-down' also in visual state
    "C-d" 'evil-scroll-down)
  (my/insert-state-keys
    :keymaps 'comint-mode-map
    ;; enable default binding for previous-input
    "C-p" 'comint-previous-input)
  (general-unbind compilation-mode-map
    ;; unbind 'describe-mode' to prevent conflict with evil navigation
    ;; ('describe-mode' is also available via "C-h m" or "g?")
    "h")
  (my/normal-state-keys
    :keymaps 'compilation-mode-map
    ;; reset 'recompile' to prevent conflicts with evil-mc (gr)
    "gR" 'recompile)
  (my/normal-state-keys
    :keymaps 'dired-mode-map
    "gR" 'revert-buffer)
  (my/normal-state-keys
    :keymaps 'package-menu-mode-map
    "?" 'package-menu-describe-package))

(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "fd")
  (evil-escape-delay 0.2)
  :config (evil-escape-mode))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)           ; enable evil snipe mode
  (evil-snipe-override-mode 1)) ; enable alternate behaviour for "f/t/F/T" keys

(use-package evil-surround
  :after (evil evil-snipe)
  :config
  ;; swap evil-surround default behavior:
  ;; - use non-spaced pairs when surrounding with an opening brace
  ;; - use spaced pairs when surrounding with a closing brace
  (evil--add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }"))
  :general
  ;; bind manually instead of using 'global-evil-surround-mode' to prevent conflicts with evil-snipe
  (my/visual-state-keys
    "gS" 'evil-Surround-region
    "gs" 'evil-surround-region))

(use-package evil-commentary
  :after evil
  :general
  ;; use this instead of 'evil-commentary-mode' to defer loading
  (my/normal-state-keys "gc" 'evil-commentary))

(use-package evil-mc
  :after evil
  :config (global-evil-mc-mode 1))

;;;* parentheses

;; highlighting matching parentheses
(use-package paren
  :ensure nil
  :custom-face
  (show-paren-match-expression ((t (:weight ultra-bold))))
  :config
  (setq show-paren-style 'expression)
  (show-paren-mode))

;; insert closing delimiter automatically
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-local-mode)))

;; mark nested parentheses with different colors
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :custom-face
  ;; make the color scheme more colorful
  (rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"
					:weight semi-bold))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "dark red"
					:weight semi-bold))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "deep sky blue"
					:weight semi-bold))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "magenta"
					:weight semi-bold))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "dim gray"
					:weight semi-bold))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "dark green"
					:weight semi-bold))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "firebrick"
					:weight semi-bold))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "medium violed red"
					:weight semi-bold))))
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;;* windows

;; mark windows with numbers for easier navigation
(use-package winum
  :init
  ;; use the 'which-key-replacement-alist' to bundle 'winum-select-window-x' functions in which-key to one entry
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
	which-key-replacement-alist)
  (push '((nil . "winum-select-window-[1-9]") . t)
	which-key-replacement-alist)
  :custom
  (winum-scope 'frame-local)
  (winum-auto-setup-mode-line nil) ; do not add the window number to the modeline autmatically
  :config
  ;; we don't want to create autoloads here and therefore skip the ':general' keyword
  (my/leader-key
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9)
  (winum-mode))

;; window utility functins
(use-package windmove
  :ensure nil
  :config
  (my/leader-key
    :infix my/infix/windows
    "w" '(:ignore t :which-key "Swap")
    "wh" '(windmove-swap-states-left :which-key "left")
    "wj" '(windmove-swap-states-down :which-key "down")
    "wk" '(windmove-swap-states-up :which-key "up")
    "wl" '(windmove-swap-states-right :which-key "right")
    "dh" '(windmove-delete-left :which-key "left")
    "dj" '(windmove-delete-down :which-key "down")
    "dk" '(windmove-delete-up :which-key "up")
    "dl" '(windmove-delete-right :which-key "right")))

;;;* tabs

;; Tabs are native feature of Emacs since version 27

;; store window configurations in tabs
(use-package tab-bar-mode
  :ensure nil
  :init
  (defun my/get-tab-name ()
    "Return the name of the current tab."
    (let* ((tabs (funcall tab-bar-tabs-function))
	   (tab-index (or current-prefix-arg (1+ (tab-bar--current-tab-index tabs))))
	   (tab-name (alist-get 'name (nth (1- tab-index) tabs))))
      tab-name))
  ;; show the current tab name as the window name
  (setq-default frame-title-format '(:eval (format "%s" (my/get-tab-name))))
  :custom (tab-bar-show nil) ; never display the tab bar
  :general
  (my/leader-key
    :infix my/infix/tabs
    "n" 'tab-bar-new-tab
    "d" 'tab-bar-close-tab
    "t" 'tab-bar-select-tab-by-name
    "r" 'tab-bar-rename-tab))

;;;* ivy, counsel & swiper

(use-package ivy
  :custom
  (ivy-count-format "(%d/%d) ")          ; format for the number of candidates
  (ivy-use-virtual-buffers t)            ; enable virtual buffers (e.g. recent files & bookmarks)
  (ivy-magic-slash-non-match-action nil) ; allow "/" to create new non-existent directories
  (ivy-use-selectable-prompt t)          ; makes the prompt line (line 0) selectable
  (ivy-fixed-height-minibuffer t)        ; fixate the height of the minibuffer even if there are fewer candidates
  :config
  (defun my/ivy-switch-to-non-star-buffer ()
    "Call 'ivy-switch-buffer' but ignore all buffers containing a star (*)."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers `("^\*"))))
      (ivy-switch-buffer)))
  (my/leader-key
    "bb" '(ivy-switch-buffer :which-key "switch")
    "bB" '(my/ivy-switch-to-non-star-buffer :which-key "switch (no *)"))

  ;; enable up and down navigation in ivy buffer with 'C-j' and 'C-k'
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line)
  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-k" 'ivy-previous-line
   "C-d" 'ivy-switch-buffer-kill) ; kill a buffer directly from the ivy list

  (ivy-mode 1))

(use-package ivy-xref
  :after ivy
  :init
  ;; xref initialization is different in Emacs 27
  ;; there are two different variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; necessary in Emacs <27. In Emacs 27 it will affect all xref-based commands
  ;; other than xref-find-definitions (e.g. project-find-regexp) as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :after ivy
  :custom (recentf-max-saved-items 50) ; increase number of saved recent files (default: 20)
  :general
  (my/leader-key "SPC" '(counsel-M-x :which-key "M-x"))
  (my/leader-key
    :infix my/infix/files
    "f" '(counsel-find-file :which-key "find file")
    "r" '(counsel-recentf :which-key "open recent file"))
  (my/leader-key
    :infix my/infix/insert
    "p" '(counsel-yank-pop :which-key "from clipboard")
    "c" '(my/insert-color-hex :which-key "color"))
  (my/leader-key
    :infix my/infix/search
    "f" '(my/counsel-in-dir :which-key "search in dir"))
  (my/leader-key
    :infix my/infix/jump
    "i" '(counsel-semantic-or-imenu :which-key "imenu"))
  :config
  (defun my/insert-color-hex ()
    "Insert a W3C color hex code."
    (interactive)
    (let* ((ivy-inhibit-action t) ; set 'ivy-inhibit-action' to prevent any ivy action
	   (color (counsel-colors-web)))
      (counsel-colors-action-insert-hex color)))

  (defun my/counsel-in-dir ()
    "Use 'counsel-ag' inside a user choosen directory."
    (interactive)
    ;; skip ag arguments by passing 'nil'
    (counsel-ag nil (counsel-read-directory-name "Search directory: ")))

  (counsel-mode 1))

(use-package swiper
  :after ivy
  :custom (swiper-goto-start-of-match t) ; put cursor on match start (instead of the end)
  :general
  (my/leader-key
    :infix my/infix/search
    "s" '(swiper :which-key "search in current file"))
  (my/leader-key
    :states 'visual
    :infix my/infix/search
    "s" '(my/swiper-with-input :which-key "search in current file"))
  :config
  (defun my/swiper-with-input (start end)
    "Use 'Swiper' for search but with an inital input."
    (interactive "r")
    (let ((region-string (buffer-substring start end)))
      (swiper region-string))))

(use-package wgrep
  :after (ivy counsel)
  :commands ivy-wgrep-change-to-wgrep-mode
  :general
  (my/major-mode-leader-key
    :keymaps 'ivy-occur-mode-map
    :major-modes t
    "" '(:ignore t :which-key "Ivy Occur")
    "y" '(my/copy-content-to-new-buffer :which-key "copy to new buffer"))
  ;; add major mode bindings to switch easily to wgrep mode
  (my/major-mode-leader-key
    :keymaps 'ivy-occur-grep-mode-map
    :major-modes t
    "" '(:ignore t :which-key "Ivy Occur")
    "y" '(my/copy-content-to-new-buffer :which-key "copy to new buffer")
    "w" '(ivy-wgrep-change-to-wgrep-mode :which-key "enable wgrep"))
  :config
  (defun my/copy-content-to-new-buffer ()
    "Copy content of the current buffer into a new one and kill the current."
    (interactive)
    (let ((old-buffer (current-buffer))
	  (new-buffer (generate-new-buffer "untitled")))
      (copy-to-buffer new-buffer (point-min) (point-max))
      (switch-to-buffer new-buffer)
      (text-mode)
      (kill-buffer old-buffer))))

;; style utilities to make ivy minibuffers more pretty
(use-package ivy-rich
  :after (ivy all-the-icons)
  :defines all-the-icons-dir-icon-alist
  ;; set minibuffer local tab-width for correct icon alignment
  :hook ((minibuffer-setup . (lambda () (setq-local tab-width 2))))
  :custom
  (ivy-rich-path-style 'abbrev)
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-switch-buffer-icon (:width 2))
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success)) ; return project name using 'projectile'
       ;; return file path relative to project root or 'default-directory' if project is nil
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer (:width 45))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
      :delimiter "\t")
     counsel-find-file
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate (:width 0.8))))))
  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    "Display the appropriate icon for the 'major-mode' of CANDIDATE."
    (with-current-buffer
	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode)
	  icon))))

  ;; [src: https://ladicle.com/post/config/]
  (defun ivy-rich-file-icon (candidate)
    "Display the appropriate icon for file type of CANDIDATE."
    (when (display-graphic-p)
      (let ((icon (if (file-directory-p candidate)
		      (cond
		       ((and (fboundp 'tramp-tramp-file-p)
			     (tramp-tramp-file-p default-directory))
			(all-the-icons-octicon "file-directory"))
		       ((file-symlink-p candidate)
			(all-the-icons-octicon "file-symlink-directory"))
		       ((all-the-icons-dir-is-submodule candidate)
			(all-the-icons-octicon "file-submodule"))
		       ((file-exists-p (format "%s/.git" candidate))
			(all-the-icons-octicon "repo"))
		       (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
			    (apply (car matcher) (list (cadr matcher))))))
		    (all-the-icons-icon-for-file candidate))))
	icon)))
  (ivy-rich-mode 1))

;;;* projects

;; project management utility
(use-package projectile
  :defer 1
  :config
  ;; special configuration for Dart projects
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
  (my/leader-key
    :infix my/infix/projects
    "d" '(projectile-kill-buffers :which-key "kill all project buffers"))
  (projectile-mode +1))

;; counsel integration for projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :general
  (my/leader-key
    :infix my/infix/projects
    "p" '(counsel-projectile-switch-project :which-key "switch project")
    "b" '(counsel-projectile-switch-to-buffer :which-key "switch to project buffer")
    "f" '(counsel-projectile-find-file :which-key "find file")
    "s" '(counsel-projectile-ag :which-key "search in project"))
  :config
  (general-define-key
   :keymaps 'counsel-projectile-switch-to-buffer-map
   "C-d" "C-c C-k") ; kill project buffers (similar to 'ivy-switch-buffer-kill')
  (counsel-projectile-mode))

;;;* company

;; autocompletion framework for emacs
(use-package company
  :hook ((prog-mode . company-mode)
	 (ielm-mode . company-mode))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  ;; allows you to keep on typing even if no completion candidate matches
  (company-require-match nil)
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'evil-complete-next               ; skip company and use evil complete next instead
   "C-p" 'evil-complete-previous           ; skip company and use evil complete prev instead
   "<return>" 'company-complete-selection) ; use <return> to complete the completion
  ;; use <TAB> for activating company
  (general-define-key
   :keymaps 'company-mode-map
   [remap indent-for-tab-command] 'company-indent-or-complete-common)
  (general-define-key
   :keymaps 'java-mode-map
   [remap c-indent-line-or-region] 'company-indent-or-complete-common))

;;;* dired

(use-package wdired
  :ensure nil
  :general
  (my/major-mode-leader-key
    :keymaps 'wdired-mode-map
    "" '(:ignore t :which-key "Wdired")
    "c" '(wdired-finish-edit :which-key "finish edit")
    "k" '(wdired-abort-changes :which-key "cancel"))
  :config
  (evil-set-initial-state 'wdired-mode 'normal)
  ;; refresh the buffer after aborting to ensure that the icons are displayed correctly
  (advice-add 'wdired-abort-changes :after (lambda () (revert-buffer))))

(use-package dired
  :ensure nil
  :hook (dired-mode . auto-revert-mode) ; automatically revert buffer on file changes
  :custom (dired-dwim-target t)         ; make copying files with split windows easier
  :general
  (my/leader-key
    :infix my/infix/dired
    "d" '(dired :which-key "dired")
    "K" '(my/kill-all-dired-buffers :which-key "kill all dired buffers")
    "b" '(my/ivy-switch-to-dired-buffer :which-key "switch to dired buffer"))
  (my/normal-state-keys
    :keymaps 'dired-mode-map
    "_" 'my/dired-create-empty-file)
  :config
  (defun my/kill-all-dired-buffers ()
    "Kill all currently opened 'dired' buffers."
    (interactive)
    (let ((dired-buffers (-filter (-compose (-partial 'eq 'dired-mode)
					    (-partial 'buffer-local-value 'major-mode)) (buffer-list))))
      (when (yes-or-no-p (concat "Do you really want to kill all " (number-to-string (length dired-buffers)) " 'dired' buffers?"))
	(mapc 'kill-buffer dired-buffers))))

  (defun my/dired-create-empty-file ()
    "Create an empty file within a dired buffer. This function respects the current (subtree) directory."
    (interactive)
    (when (derived-mode-p 'dired-mode)
      (let ((default-directory (dired-current-directory)))
	(call-interactively 'dired-create-empty-file)
	(revert-buffer))))

  (defun my//only-dired-buffers (buffer)
    "Filter function to ignore all 'non-dired' buffers."
    (when (get-buffer buffer)
      (with-current-buffer buffer
	(not (eq major-mode 'dired-mode)))))

  (defun my/ivy-switch-to-dired-buffer ()
    "Call 'ivy-switch-buffer' but show only currently opened dired buffers."
    (interactive)
    (let ((ivy-ignore-buffers (append ivy-ignore-buffers '(my//only-dired-buffers))))
      (ivy-switch-buffer))))

;; show icons for files and directories
(use-package all-the-icons-dired
  :after dired all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; multi stage copy/pasting of files
(use-package dired-ranger
  :after dired
  :general
  (my/normal-state-keys
   :keymaps 'dired-mode-map
   :prefix "C-r"
   "" '(:ignore t :which-key "Dired-Ranger")
   "y" '(dired-ranger-copy :which-key "copy")
   "Y" '(my/dired-ranger-copy-add :which-key "add to copy")
   "m" '(dired-ranger-move :which-key "move")
   "p" '(my/dired-ranger-paste :which-key "paste"))
  :config
  (defun my/dired-ranger-copy-add ()
    "Call 'dired-ranger-copy' with prefix arg to ad the selected files to the last copy ring entry."
    (interactive)
    (dired-ranger-copy '(4)))

  (defun my/dired-ranger-paste ()
    "Call 'dired-ranger-paste' with prefix arg to prevent the clipboard to be cleared."
    (interactive)
    (dired-ranger-paste '(4))))

;; filtering of files in dired buffers
(use-package dired-narrow
  :after dired
  :general
  (my/normal-state-keys
    :keymaps 'dired-mode-map
    "/" 'dired-narrow))

;; render directory subtree inside dired buffer
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "->")
  :config
  (defun my/dired-subtree-toggle ()
    "Refresh buffer after toggeling the subtree to ensure the icons are loaded correctly."
    (interactive)
    (dired-subtree-toggle)
    (revert-buffer))

  (my/normal-state-keys
    :keymaps 'dired-mode-map
    "TAB" 'my/dired-subtree-toggle))

;; neotree like sidebar using dired
(use-package dired-sidebar
  :after dired dired-subtree
  :custom
  (dired-sidebar-refresh-on-projectile-switch nil) ; do not refresh the sidebar on project switch
  (dired-sidebar-toggle-hidden-commands nil)       ; don't hide sidebar during certain commands (caused problems with 'balance-windows')
  :general
  (my/leader-key
    "d" '(my/dired-side-bar-toggle :which-key my//sidebar-which-key-replacement))
  :init
  (defun my//sidebar-which-key-replacement (entry)
    "Which key replacement function for the 'dired-sidebar'."
    (let ((key (car entry)))
      (if (and
	   (fboundp 'dired-sidebar-showing-sidebar-p)
	   (dired-sidebar-showing-sidebar-p))
	`(,key . "sidebar close")
	`(,key . "sidebar open"))))
  :config
  ;; do not resize the dired-sidebar window after toggeling
  (add-to-list 'window-size-change-functions
                  (lambda (_)
                    (let ((sidebar-window (dired-sidebar-showing-sidebar-p)))
                      (unless (null sidebar-window)
                        (setq dired-sidebar-width (window-width sidebar-window))))))

  ;; remap sidebar specific functions
  (my/normal-state-keys
    :keymaps 'dired-sidebar-mode-map
    [remap dired-up-directory] 'dired-sidebar-up-directory
    [remap quit-window] 'dired-sidebar-toggle-sidebar)

  ;; map sidebar window wo window number zero
  (with-eval-after-load 'winum
    (defun winum-assign-0-to-sidebar ()
      "Always assign any 'dired-sidebar' window to winum number zero."
      (when (eq major-mode 'dired-sidebar-mode) 0))
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-sidebar))

  (defun my/dired-side-bar-toggle ()
    "Toggle the 'dired-sidebar'. Also unlock the fixed window size of the sidebar window."
    (interactive)
    (dired-sidebar-toggle-sidebar)
    (when (dired-sidebar-showing-sidebar-p)
      ;; unlock fixed sidebar window width
      (setq-local window-size-fixed nil))))

;;;* move around

;; move lines/selected text up and down easily
(use-package drag-stuff
  :general
  (my/visual-state-keys
    "C-S-h" 'drag-stuff-left
    "C-S-l" 'drag-stuff-right)
  (my/all-states-keys
    "C-S-j" 'drag-stuff-down
    "C-S-k" 'drag-stuff-up)
  :config (drag-stuff-mode t))

;; jump to text/words/lines
(use-package avy
  :general
  (my/leader-key
    :infix my/infix/jump
    "j" '(avy-goto-char-timer :which-key "avy timer")
    "l" '(avy-goto-line :which-key "avy line")))

;;;* flycheck

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :general
  (my/leader-key
    :infix my/infix/toggle
    "f" '(flycheck-mode :which-key "flycheck"))
  (my/leader-key
    :infix my/infix/errors
    :keymaps 'flycheck-mode-map
    "" '(hydra-flycheck/body :which-key "~Errors~"))
  :init
  ;; always display the error list at the bottom side of the frame
  ;; occupying a third of the entire height of the frame
  ;; source: flycheck docs (PDF)
  (add-to-list 'display-buffer-alist
	       `(, (rx bos "*Flycheck errors*" eos)
		   (display-buffer-reuse-window
		    display-buffer-in-side-window)
		   (side . bottom)
		   (reusable-frames . visible)
		   (window-height . 0.33)))
  :config
  (defhydra hydra-flycheck (:hint nil)
    "
^Navigation^         ^Other^
^^^^--------------------------------
_n_: next error      _L_: error list
_N_: previous error
_z_: center
^^^^--------------------------------
[_q_]: quit
^^^^--------------------------------
"
    ("n" flycheck-next-error)
    ("N" flycheck-previous-error)
    ("L" flycheck-list-errors :exit t)
    ("z" evil-scroll-line-to-center)
    ("q" nil :color blue)))

;;;* snippets

;; basic package for snippet insertion
(use-package yasnippet
  :general
  (my/leader-key
    :infix my/infix/insert
    "s" '(yas-insert-snippet :which-key "snippet"))
  :config (yas-global-mode 1))

;; collection of general useful snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; java specific snippets
(use-package java-snippets
  :after yasnippet)

;;;* git

;; use GIT with ease from within Emacs
(use-package magit
  :commands (magit-status magit-blame)
  :general
  (my/leader-key
    :infix my/infix/git
    "s" '(magit-status :which-key "magit status")
    "b" '(magit-blame :which-key "magit blame"))

  (my/major-mode-leader-key
    :keymaps 'with-editor-mode-map
    :major-modes 'text-mode
    "" '(:ignore t :which-key "Editor")
    "c" '(with-editor-finish :which-key "editor finish")
    "k" '(with-editor-cancel :which-key "editor cancel"))
  :config
  ;; this makes magit ask us wether we want to create a PR after we pushed a new branch to stash
  ;; and if so it opens the webpage for creating it in your browser
  (defvar jms/magit-process-klarna-create-pull-request-regexp
    "remote: Create pull request for.*\nremote: +\\(?1:[^ ]+\\)[^\n]*")
  
  (defun jms/magit-process-klarna-ask-create-bitbucket-pull-request (_ string)
    "Check if the STRING match the pull request regex and browse to this url if desired."
    (when (string-match jms/magit-process-klarna-create-pull-request-regexp string)
      (let ((url (match-string 1 string))
            (inhibit-message t))
	(if (y-or-n-p "Create PR? ")
            (browse-url (url-encode-url url))))))
  
  (setq magit-process-prompt-functions #'jms/magit-process-klarna-ask-create-bitbucket-pull-request))

;; evil bindings for magit
(use-package evil-magit
  :after magit)

;;;* terminal

;; quickly open a shell buffer from anywhere
(use-package shell-pop
  :commands shell-pop
  :hook (shell-pop-in-after . evil-insert-state)
  :custom
  (shell-pop-full-span t)
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  :general (my/leader-key "'" '(shell-pop :which-key "shell")))

;; copy environment variables from your local shell to emacs
(use-package exec-path-from-shell
  :defer t
  :init
  ;; only initialize in case it is needed
  ;; also prevents double initialization
  (defvar my--shell-env-variables-copied nil)
  (defun my//copy-shell-env-variables ()
    (when (not my--shell-env-variables-copied)
      (exec-path-from-shell-initialize)
      (setq my--shell-env-variables-copied t))))

;;;* org mode

(defvar my-org-notes-dir (my//get-value-from-config "org-roam-dir" "~/org-roam"))

(use-package org
  :ensure nil
  :hook ((org-mode . org-hide-block-all)) ; leave all blocks collapsed by default
  :custom
  (org-hide-emphasis-markers t)
  (org-todo-keywords '((sequence "TODO" "PROCESSING" "WAITING" "|" "DONE" "CANCELED")))
  ;; refile target all level 1-2 headings in the same file
  (org-refile-targets '((nil :maxlevel . 2)))
  :general
  ;; add 'org-capture' keybinding to the leader-key
  (my/leader-key
    "C-c" '(org-capture :which-key "org capture"))

  (my/major-mode-leader-key
    :keymaps '(org-mode-map org-journal-mode-map)
    :major-modes t
    "" '(:ignore t :which-key "Org")
    "m" '(org-ctrl-c-ctrl-c :which-key "ctrl-c ctrl-c")
    "TAB" '(my/org-show-current-heading-tidily :which-key "collapse others")
    "o" '(org-open-at-point :which-key "open at point")
    "r" '(org-refile :which-key "refile")
    ;; Toggles
    "T" '(:ignore t :which-key "Toggle")
    "Ti" 'my/org-toggle-inline-images
    ;; Insert
    "i" '(:ignore t :which-key "Insert")
    "ib" '(org-insert-structure-template :which-key "template block")
    "it" '(counsel-org-tag :which-key "set tags")
    ;; Links
    "l" '(:ignore t :which-key "Links")
    "le" '(hydra-org-links/body :which-key "~Edit~")
    "ll" '(org-insert-link :which-key "link")
    "li" '(my/insert-internal-org-link :which-key "internal link")
    "lf" '(my/insert-file-link :which-key "file link")
    ;; Tables
    "t" '(:ignore t :which-key "Tables")
    "tt" '(org-table-create-or-convert-from-region :which-key "table")
    "tb" '(org-table-blank-field :which-key "blank field")
    "te" '(hydra-org-table/body :which-key "~Edit~")
    ;; Text
    "x" '(:ignore t :which-key "Text")
    "xb" '(my/org-bold :which-key "bold")
    "xi" '(my/org-italic :which-key "italic")
    "xc" '(my/org-code :which-key "code")
    "xs" '(my/org-strike-through :which-key "strike through")
    "xu" '(my/org-underline :which-key "underline")
    "xv" '(my/org-verbatim :which-key "verbatim")
    "x SPC" '(my/org-clear :which-key "clear"))
  :config
  ;; log the creation date for a TODO item
  ;; [src: https://emacs.stackexchange.com/a/35776]
  (defun my/log-todo-creation-date (&rest _)
    "Log TODO creation time in the property drawer under the key 'CREATED'."
    (when (and (org-get-todo-state)
               (not (org-entry-get nil "CREATED")))
      (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a]"))))
  (add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)

  (defhydra hydra-org-table (:hint nil)
    "
^Column^            ^Row^               ^HLine^
^^^^^^^^---------------------------------------------------
_ch_: insert left   _rj_: insert below  _hj_: insert below
_cl_: insert right  _rk_: insert above  _hk_: insert above
_cd_: delete
^^^^^^^^---------------------------------------------------
[_q_]: quit
^^^^^^^^---------------------------------------------------
"
    ("ch" my/org-insert-column-left)
    ("cl" org-table-insert-column)
    ("cd" org-table-delete-column)
    ("rj" my/org-insert-row-below)
    ("rk" org-table-insert-row)
    ("hj" org-table-insert-hline)
    ("hk" my/org-insert-hline-above)
    ("q" nil))

  (defhydra hydra-org-links (:hint nil)
    "
^^Description
^^------------------
_b_: barf forward
_B_: barf backward
_s_: slurp forward
_S_: slurp backward
^^------------------
[_q_]: quit
^^------------------
"
    ("b" org-link-edit-forward-barf)
    ("B" org-link-edit-backward-barf)
    ("s" org-link-edit-forward-slurp)
    ("S" org-link-edit-backward-slurp)
    ("q" nil))
  
  ;;; ##### functions & variables
  (defun my/org-insert-column-left ()
    "Insert a new table column left of the current one."
    (interactive)
    (org-table-insert-column)
    (org-table-move-column-left))
  
  (defun my/org-insert-row-below ()
    "Insert a new table row below the current one."
    (interactive)
    (org-table-insert-row t))
  
  (defun my/org-insert-hline-above ()
    "Insert a new table hline above the current row."
    (interactive)
    (org-table-insert-hline)
    (org-table-move-row-down))
  
  (defun my/org-bold ()
    "Mark the selected region as bold."
    (interactive)
    (org-emphasize ?*))
  
  (defun my/org-italic ()
    "Mark the selected region as italic."
    (interactive)
    (org-emphasize ?/))
  
  (defun my/org-code ()
    "Mark the selected region as code."
    (interactive)
    (org-emphasize ?~))
  
  (defun my/org-strike-through ()
    "Mark the selected region as strike through."
    (interactive)
    (org-emphasize ?+))
  
  (defun my/org-underline ()
    "Mark the selected region as underlined."
    (interactive)
    (org-emphasize ?_))
  
  (defun my/org-verbatim ()
    "Mark the selected region as verbatim."
    (interactive)
    (org-emphasize ?=))
  
  (defun my/org-clear ()
    "Clear the selected region."
    (interactive)
    (org-emphasize ? ))
  
  (defun my/insert-internal-org-link ()
    "Insert an internal org link to either a headline or a NAME tag."
    (interactive)
    (let* (;; regex to remove all parent headings and org todo keywords (e.g. ".*/(TODO|DONE)( )?")
	   (todo-regex (concat ".*/\\(" (mapconcat 'identity
						   (seq-filter 'stringp (apply #'append org-todo-keywords))
						   "\\|") "\\)?\\( \\)?"))
	   ;; regex for the NAME tags
	   (name-regex "#\\+NAME:\\( \\)*")
	   (headlines (seq-map #'remove-all-text-properties (seq-map #'-first-item (counsel-outline-candidates))))
	   (names (seq-map #'-first-item (s-match-strings-all
					  (concat name-regex ".*$")
					  (buffer-string))))
	   (target (ivy-read "Destination: " (append headlines names)))
	   (link-target (replace-regexp-in-string
			 ;; replace the prefix to get a correct link target
			 (concat "\\(" todo-regex "\\|" name-regex "\\)") "" target))
	   (description (read-string "Description: ")))
      (if (string= "" description)
	  (insert (concat "[[" link-target "]]"))
	(insert (concat "[[" link-target "][" description "]]")))))
  
  (defun my/insert-file-link ()
    "Insert an org link to a file."
    (interactive)
    (org-insert-link '(4)))
  
  (defun my/org-toggle-inline-images ()
    "Call 'org-toggle-inline-images' with prefix arg."
    (interactive)
    (org-toggle-inline-images t))
  
  ;; [src: https://stackoverflow.com/q/25161792]
  (defun my/org-show-current-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
	(progn (org-show-entry) (outline-show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-at-heading-p))
	(org-up-heading-safe)
	(outline-hide-subtree)
	(error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (outline-show-children)))

  ;;; ##### keybindings
  ;;; ##### configuration
  ;; 'kill-ring' adaptions (special case for 'org-delete-char')
  ;; since 'evil-org-delete-char' can use 'org-delete-char' internally as a fallback, we have to advice the function in order
  ;; to prevent it from adding to the 'kill-ring', we do so by manually removing the last entry from the 'kill-ring' afterwards
  (advice-add 'org-delete-char :after '(lambda (&rest _) (setq kill-ring (cdr kill-ring)))))

(use-package evil-org
  :after (org evil)
  :functions evil-org-agenda-set-keys
  :hook ((org-mode . evil-org-mode))
  :config
  ;;; ##### functions & variables
  (defun my/org-insert-subheading ()
    "Insert a subheading and switch to insert mode"
    (interactive)
    (evil-org-append-line 1)
    (org-insert-subheading nil))

  ;;; ##### keybindings
  ;; this has to be called before all other keybindings, otherwise it could overwrite them
  (evil-org-set-key-theme '(navigation additional textobjects calendar))
  (my/normal-state-keys
    :keymaps 'org-agenda-mode-map
    "M-l" 'org-agenda-later
    "M-h" 'org-agenda-earlier)
  (my/normal-state-keys
    :keymaps 'evil-org-mode-map
    "C-S-l" 'org-shiftright
    "C-S-h" 'org-shiftleft)
  (my/insert-state-keys
    :keymaps 'evil-org-mode-map
    "C-S-l" 'org-shiftright
    "C-S-h" 'org-shiftleft
    "M-l" 'org-metaright
    "M-h" 'org-metaleft
    "M-k" 'org-metaup
    "M-j" 'org-metadown
    "M-o" 'evil-org-open-below
    "M-O" 'evil-org-open-above)
  (my/major-mode-leader-key
    :keymaps 'evil-org-mode-map
    "ih" '(evil-org-org-insert-heading-respect-content-below :which-key "heading")
    "is" '(my/org-insert-subheading :which-key "subheading"))

  ;;; ##### configuration
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-journal
  ;; carry over all items that do not mark an entry as done
  ;; either all 'org-todo-keywords' before the "|" entry or all but the last keyword
  :custom
  (org-journal-dir (my//get-value-from-config "org-journal-dir" "~/Documents/journal/"))
  (org-journal-carryover-items
   (let ((keywords (cdr (-first-item org-todo-keywords)))) ; extract the keyword strings from the org variable
     (s-join "|" (-map (lambda (x) (concat "TODO=\"" x "\""))
		       (if (-contains? keywords "|")
			 (-take-while (-compose 'not (-partial 's-equals? "|")) keywords)
			 (-butlast keywords))))))
  :general
  (my/leader-key
    "J" '(:ignore t :which-key "Journal")
    "Jn" '(org-journal-new-entry :which-key "new entry")
    "Jc" '(my/journal-view-current-entry :which-key "current entry")
    "Js" '(org-journal-new-scheduled-entry :which-key "scheduled entry"))
  (my/leader-key
    :keymaps 'org-journal-mode-map
    "JJ" '(hydra-journal/body :which-key "navigate"))
  :config
  (defun my/journal-view-current-entry ()
    "Call 'org-journal-new-entry' with a prefix to not create a new entry"
    (interactive)
    (org-journal-new-entry t))
  (defhydra hydra-journal ()
    ""
    ("n" org-journal-next-entry "next entry")
    ("N" org-journal-previous-entry "previous entry")
    ("q" nil "quit")))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory my-org-notes-dir) ; set the directory for 'org-roam' to operate
  :config
  ;;; ##### functions & variables
  ;;; ##### configuration
  ;;; ##### keybindings
  (my/leader-key
    :infix "o"
    :keymaps 'org-roam-mode-map
    "" '(:ignore t :which-key "Roam")
    "o" '(org-roam :which-key "toggle roam buffer")
    "f" '(org-roam-find-file :which-key "find file")
    "g" '(org-roam-show-graph :which-key "show graph"))
  (my/leader-key
    :infix "o"
    :keymaps 'org-mode-map
    "i" '(org-roam-insert :which-key "insert link")))

(use-package deft
  :general
  (my/leader-key
    :infix "o"
    "s" '(deft :which-key "search in notes (deft)"))
  :custom
  (deft-directory my-org-notes-dir) ; set the directory for 'deft' to operate
  (deft-extensions '("org"))        ; only consider .org files
  ;; remove certain elements of org files for the summary
  (deft-strip-summary-regexp
    (concat "\\("
           "[\n\t]"                    ; blank lines
           "\\|^#\\+[[:upper:]_]+:.*$" ; org-mode metadata
	   "\\|^- tags :: .*$"         ; org roam tag line
           "\\)"))
  :config
  ;;; ##### functions & variables
  ;;; ##### keybindings
  (my/normal-state-keys
    :keymaps 'deft-mode-map
    "q" 'kill-current-buffer)
  (my/insert-state-keys
    :keymaps 'deft-mode-map
    "C-g" 'kill-current-buffer
    "C-j" 'next-line
    "C-k" 'previous-line)

  ;;; ###### configuration
  ;; set default evil state for 'deft' to 'insert' so we can directly start typing
  (evil-set-initial-state 'deft-mode 'insert))

;;;* programming

;;;** language server protocol (LSP)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-file-watchers nil)       ; disabe file watchers to prevent warning about too many files
  (lsp-headerline-breadcrumb-enable t) ; show breadcrumbs in headerline
  :init
  (defmacro my/lsp-keybindings (keymap name &rest additional-bindings)
    "Set the default LSP keybindings for the given 'major-mode' KEYMAP.
Name the prefix for which-key according to the NAME string argument.
You can pass in ADDITIONAL-BINDINGS to add mode specific behavior or to overwrite unsupported pre-defined bindings."
    `(my/major-mode-leader-key
       :keymaps ,keymap
       "" '(:ignore t :which-key ,name)
       :major-modes t
       "a" '(lsp-execute-code-action :which-key "code action")
       "h" '(:ignore t :which-key "Help/Docs")
       "hh" '(lsp-ui-doc-glance :which-key "at point")
       "=" '(:ignore t :which-key "Formatting")
       "==" '(lsp-format-buffer :which-key "whole buffer")
       "=r" '(lsp-format-region :which-key "region")
       "w" '(:ignore t :which-key "Windows")
       "ws" '(lsp-treemacs-symbols :which-key "symbols")
       "we" '(lsp-treemacs-errors-list :which-key "errors")
       "wc" '(lsp-treemacs-call-hierarchy :which-key "call hierarchy")
       "T" '(:ignore t :which-key "Toggle")
       "Tl" '(lsp-toggle-trace-io :which-key "lsp logging")
       "Ts" '(lsp-ui-sideline-mode :which-key "sideline")
       "Td" '(lsp-ui-doc-mode :which-key "docs")
       "Th" '(lsp-toggle-symbol-highlight :which-key "symbol highlighting")
       "Tt" '(lsp-treemacs-sync-mode :which-key "treemacs sync")
       "g" '(:ignore t :which-key "Goto")
       "gb" '(xref-pop-marker-stack :which-key "jump back")
       "gG" '(my/jump-to-definition-other-window :which-key "definition other window")
       "gR" '(lsp-ui-peek-find-references :which-key "peek references")
       "gg" '(lsp-find-definition :which-key "definition")
       "gi" '(lsp-find-implementation :which-key "implementation")
       "gr" '(lsp-find-references :which-key "references")
       "f" '(:ignore t :which-key "Find")
       "fs" '(lsp-ivy-workspace-symbol :which-key "symbol")
       "fS" '(my/lsp-ivy-with-prefix :which-key "current symbol")
       "r" '(:ignore t :which-key "Refactoring")
       "rr" '(lsp-rename :which-key "rename")
       "ro" '(lsp-organize-imports :which-key "organize imports")
       ;; LSP related commands
       "L" '(:ignore t :which-key "LSP")
       "Lq" '(lsp-disconnect :which-key "disconnect")
       "Ld" '(lsp-describe-session :which-key "describe session")
       "Lc" '(lsp :which-key "connect")
       "Lw" '(:ignore t :which-key "Workspace")
       "Lwa" '(lsp-workspace-folders-add :which-key "add folder")
       "Lwd" '(lsp-workspace-folders-remove :which-key "remove folder")
       "Lwq" '(lsp-workspace-shutdown :which-key "shutdown")
       "Lwr" '(lsp-workspace-restart :which-key "restart")
       ;; dap-mode debug bindings
       "d" '(:ignore t :which-key "Debug")
       "d." '(dap-hydra :which-key "dap hydra")
       "d'" '(dap-ui-repl :which-key "ui repl")
       "dc" '(dap-continue :which-key "continue")
       "di" '(dap-step-in :which-key "step in")
       "do" '(dap-step-out :which-key "step out")
       "dn" '(dap-next :which-key "next")
       "db" '(:ignore t :which-key "Breakpoints")
       "dba" '(dap-breakpoint-add :which-key "add")
       "dbc" '(dap-breakpoint-condition :which-key "condition")
       "dbd" '(dap-breakpoint-delete :which-key "delete")
       "dbD" '(dap-breakpoint-delete-all :which-key "delete all")
       "dbt" '(dap-breakpoint-toggle :which-key "toggle")
       "de" '(:ignore t :which-key "Eval")
       "dee" '(dap-eval :which-key "eval")
       "der" '(dap-eval-region :which-key "region")
       "det" '(dap-eval-thing-at-point :which-key "at point")
       "dw" '(:ignore t :which-key "Windows")
       "dwl" '(dap-ui-locals :which-key "locals")
       "dws" '(dap-ui-sessions :which-key "session")
       "dwb" '(dap-ui-breakpoints :which-key "breakpoints")
       "dd" '(:ignore t :which-key "Debugging")
       "ddd" '(dap-debug :which-key "debug")
       "ddl" '(dap-debug-last :which-key "debug last")
       "ddD" '(dap-delete-all-sessions :which-key "delete all session")
       ,@additional-bindings))
  :config
  (defun my/jump-to-definition-other-window ()
    "Jump to definition around point in other window."
    (interactive)
    (let ((pos (point)))
      (switch-to-buffer-other-window (current-buffer))
      (goto-char pos)
      (lsp-find-definition))))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable nil) ; disable side line view by default
  (lsp-ui-doc-enable nil))     ; disable doc mode by default

;; ivy integration for lsp (especially for symbol search)
(use-package lsp-ivy
  :after lsp-mode
  :config
  (defun my/lsp-ivy-with-prefix ()
    "Call 'lsp-ivy-workspace-symbol' with prefix argument to seach for the current symbol at point."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'lsp-ivy-workspace-symbol))))

;; debugging utility mode
(use-package dap-mode
  :after lsp-mode
  ;; 'goto-address-mode' enables mouse clicks on links shown to
  ;; open them in the browser (very convenient for redirect URLs)
  ;; [src: https://emacs.stackexchange.com/a/27100]
  :hook ((dap-server-log-mode . (lambda () (goto-address-mode 1))))
  :config
  (my/normal-state-keys
    :keymaps 'dap-server-log-mode-map
    "<mouse-1>" 'browse-url-at-mouse
    "gx" 'browse-url-at-point)
  (dap-mode t)
  (dap-ui-mode t))

;;;** javacript

(use-package js
  :ensure nil
  :hook ((js-mode . my/turn-on-js-lsp))
  :init
  (defun my/turn-on-js-lsp ()
    "Start 'lsp' but prevent the startup for derived js-modes where we don't need or want LSP."
    (let ((exceptions '(json-mode)))
      (unless (apply 'derived-mode-p exceptions)
	;; we need to make sure that all env variables from our regular shell are present
	;; this is especially needed to make the JS language server work with nvm
	(my//copy-shell-env-variables)
	(lsp))))

  (my/lsp-keybindings
   'js-mode-map
   "Javascript"
   ;; unbind unsupported operations
   "=r" nil
   "wc" nil))

;;;** json

(use-package json-mode
  :init
  ;; make a new empty keymap and set to parent to something other than 'js-mode-map'
  ;; to prevent lsp and true-/javascript keybindings in our json-mode-map
  (defvar json-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map prog-mode-map)
      map))
  :commands json-mode
  :custom (json-reformat:indent-width 2)
  :config
  (my//copy-shell-env-variables)
  ;; define the json schema validation cli command with placeholders within your config file 'my-config-file' as 'jsonschema-cmd'
  ;; -> jsonschema -i <json> <schema>
  ;; -> ajv validate -s <schema> -d <json>
  (defvar my--last-jsonschema nil)
  (defun my/json/schema-validation ()
    "Validate the current json file against a choosen json schema."
    (interactive)
    (let ((jsonschema-cmd (my//get-value-from-config "jsonschema-cmd")))
      (if (not (buffer-file-name))
	(error "The current buffer is not visiting any file! To perform JSON schema validation you need to save the buffer content to a .json file")
	(if (or (not (equal major-mode 'json-mode))
		(not (s-ends-with? ".json" (buffer-file-name))))
	  (error "Current file does not seem to contain JSON! Check for the correct file extension ('.json') and major-mode ('json-mode')")
	  (let* ((json-file (buffer-file-name))
		 (schema (counsel--find-file-1 "JSON schema: "      ; custom prompt message
					       my--last-jsonschema  ; target last jsonschema file if present
					       nil                  ; do not open the file, we just want the name of it
					       'counsel-find-file)) ; default value
		 (validation-cmd (concat (s-replace-regexp
					  "<schema>" schema
					  (s-replace-regexp
					   "<json>" json-file
					   jsonschema-cmd))))
		 (info-cmd (concat "echo Validating "
				   "\\'" (file-name-nondirectory json-file) "\\'"
				   " with JSON schema "
				   "\\'" (file-name-nondirectory schema) "\\':"
				   " && echo"))) ; print a newline between info and output
	    ;; save the last used JSON schema file for repeated usage
	    (setq my--last-jsonschema schema)
	    (with-output-to-temp-buffer "*json schema validation*"
	      (shell-command (concat info-cmd " && " validation-cmd) "*json schema validation*")))))))

  (my/major-mode-leader-key
    :keymaps 'json-mode-map
    :major-modes t
    "" '(:ignore t :which-key "Json")
    "v" '(my/json/schema-validation :which-key "schema validation")))

;;;** others (YAML, Terraform, Groovy)

(use-package yaml-mode
  :defer t)

(use-package terraform-mode
  :defer t)

(use-package groovy-mode
  :defer t)

;;;** web (HTML, CSS, etc.)

;; syntac highlighting and utility commands for HTML and CSS
(use-package web-mode
  :commands web-mode
  :general
  (my/major-mode-leader-key
    :keymaps 'web-mode-map
    :major-modes t
    "" '(:ignore t :which-key "Web")
    "TAB" '(web-mode-fold-or-unfold :which-key "fold/unfold")
    "e" '(:ignore t :which-key "Elements")
    "ei" '(web-mode-element-insert :which-key "insert")
    "ed" '(web-mode-element-kill :which-key "delete")
    "ew" '(web-mode-element-wrap :which-key "wrap")
    "es" '(web-mode-element-select :which-key "select")
    "er" '(web-mode-element-rename :which-key "rename"))
  (my/normal-state-keys
    :keymaps 'web-mode-map
    :major-modes t
    "gj" 'web-mode-element-next
    "gk" 'web-mode-element-previous)
  (my/visual-state-keys
    :keymaps 'web-mode-map
    :major-modes t
    "gj" 'web-mode-element-next
    "gk" 'web-mode-element-previous)
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode)))

;;;** java

(use-package lsp-java
  :hook ((java-mode . lsp))
  :init
  (my/lsp-keybindings
   'java-mode-map
   "Java"
   ;; additional refactoring
   "ri" '(lsp-java-add-import :which-key "add import")
   "rc" '(lsp-java-convert-to-static-import :which-key "convert to static import")
   ;; additional dependency window
   "wd" '(lsp-treemacs-java-deps-list :which-key "dependencies")
   ;; additional test methods
   "t" '(:ignore t :which-key "Tests")
   "tt" '(dap-java-run-test-method :which-key "run test method")
   "tc" '(dap-java-run-test-class :which-key "run test class")
   ;; additional debug test methods
   "dt" '(:ignore t :which-key "Tests")
   "dtt" '(dap-java-debug-test-method :which-key "debug test method")
   "dtc" '(dap-java-debug-test-class :which-key "debug test class"))

  (defun my//java/execute-task (task wrapper-path)
    "Execute a given gradle TASK with the gradle-wrapper found at WRAPPER-PATH."
    (let ((command (concat "cd " wrapper-path " && ./gradlew " task "\n"))
	  (shell-buffer-name (concat "*gw:" (projectile-project-name) ":" task "*")))
      (comint-send-string (get-buffer-process (shell shell-buffer-name)) command)
      (evil-force-normal-state) ; since we usually just want to watch the logs we switch back to normal state
      (goto-char (point-max))))	; moves to the end of the buffer in order to see the most recent logs
  
  (defun my//java/get-gradle-wrapper-path ()
    "Get the path of the project gradle-wrapper.
If multiple candidates occure, show a seletion via ivy"
    (if (not (projectile-project-root))
      (error "Not inside a project!")
      (let (;; get all gradle wrappers inside the current project
            (project-wrappers (-map (-partial 's-replace-regexp "gradlew" "")
                                    (directory-files-recursively (projectile-project-root) "^gradlew$"))))
	(if (= (length project-wrappers) 0)
	  (error "No 'gradlew' script found! Are you inside a gradlew based project?")
	  ;; if there is more than one wrapper show a selection
	  (if (> (length project-wrappers) 1)
            (ivy-read "Select the Gradle Wrapper: " project-wrappers)
            (car project-wrappers))))))
  
  (defun my//java/extract-gradle-wrapper-error (text)
    "Extract the gradle error message from TEXT."
    (-second-item (s-match "What went wrong:\n\\(.*\\)\n" text)))
  
  ;; EXAMPLE:
  ;; --> INPUT
  ;; Build Tasks
  ;; -----------
  ;; build - builds the project
  ;; test - tests the project
  ;;
  ;; --> OUTPUT
  ;; ("[Build Tasks] build - builds the project" "[Build Tasks] test - tests the project")
  (defun my//java/build-tasks-desriptions (task-group-string)
    "Takes a TASK-GROUP-STRING and return the individual task names appended by the appropriate group name."
    (let* (;; extract the task group name via regex group
	   (task-group (-second-item (s-match "^\\([a-zA-Z ]+\\)\n-+\n" task-group-string)))
           ;; extract all task names and delete all "\n"
	   (tasks (-map (-partial 's-replace-regexp "\\\n" "")
			;; map with '-first-item' to ignore the regex group values
			(-map '-first-item (s-match-strings-all "\n[a-zA-Z_0-9][a-zA-Z_0-9-]+?\\( - .*?\\)?\n" task-group-string)))))
      (-map (-partial 'concat "[" task-group "] ") tasks)))
  
  ;; EXAMPLE (for a "gradle task group"):
  ;;
  ;; Build Tasks
  ;; -----------
  ;; build - this task builds the project
  ;; test - this task tests the project
  ;; _some_task_without_documentation
  ;; [...]
  ;;
  (defun my/java/list-gradle-wrapper-tasks ()
    "Show all available gradlew tasks, select one via ivy and execute it."
    (interactive)
    (let* ((wrapper-path (my//java/get-gradle-wrapper-path))                                                ; get the gradle wrapper path
           (gradle-tasks (shell-command-to-string (concat "cd " wrapper-path " && ./gradlew tasks --all"))) ; get all gradle tasks
           (gradle-error (my//java/extract-gradle-wrapper-error gradle-tasks)))                             ; check for possible gradle error
      (if gradle-error
	(error (concat "The gradlew script threw an error: " gradle-error))
	(let* (;; split the "gradle-tasks" into multiple task-groups
               (task-groups (-map #'-first-item
				  (s-match-strings-all "^\\([a-zA-Z ]+\\)\n-+\n\\([a-zA-Z-_0-9]+?\\( - .*?\\)?\n\\)+"
						       gradle-tasks)))
               ;; append task names with appropriate group
               (tasks (-flatten (-map #'my//java/build-tasks-desriptions task-groups)))
               ;; choose a task interactively
               (chosen-task (ivy-read "Select from list: " tasks))
               ;; extract the explicit task from chosen task string
               (task-string (-second-item (s-match "\\[.*?\\] \\([^ ]*\\)\\( - .*\\)?" chosen-task))))
          (my//java/execute-task task-string wrapper-path)))))
   
  (defun my/java/execute-gradle-wrapper-task ()
    "Read a gradle-wrapper task from the input and execute it."
    (interactive)
    (let ((wrapper-path (my//java/get-gradle-wrapper-path))
          (task (read-string "Gradle Task: ")))
      (my//java/execute-task task wrapper-path)))

  (my/leader-key
    :infix my/infix/custom
    "g" '(:ignore t :which-key "Gradlew")
    "gl" '(my/java/list-gradle-wrapper-tasks :which-key "list tasks")
    "gx" '(my/java/execute-gradle-wrapper-task :which-key "execute task"))
  :config
  (defun my/java/gradle-refresh ()
    "Navigates to the projects build.gradle file and call 'lsp-java-update-project-configuration'."
    (interactive)
    (if (not (projectile-project-root))
      (error "Not inside a project!")
      (let ((build-gradle-paths (directory-files-recursively (projectile-project-root) "^build.gradle$")))
	(if (null build-gradle-paths) ; check if list is empty (empty list is equal to nil)
	  (error "No 'build.gradle' file found! Are you inside of a gradle(w) based project?")
	  (let ((buffer (current-buffer)))
	    ;; if there is more than one 'build.gradle' file show a selection
	    (if (> (length build-gradle-paths) 1)
	      (find-file (ivy-read "Select build.gradle root: " build-gradle-paths))
	      (find-file (car build-gradle-paths)))
	    (lsp-java-update-project-configuration)   ; update the project configuration (which also updates the dependencies)
	    (switch-to-buffer buffer))                ; switch window back to previous buffer
	  (switch-to-buffer-other-window "*lsp-log*") ; open the lsp-log buffer to see the outcome of the configuration update
	  (goto-char (point-max))))))                 ; move to the end of the log buffer in order to see the most recent log lines

  (my/leader-key
    :infix my/infix/custom
    "gr" '(my/java/gradle-refresh :which-key "refresh")))

(use-package dap-java
  ;; 'dap-java' is part of 'dap-mode' but needs to be loaded extra
  :ensure nil
  :after (dap-mode lsp-java))

;;;** clojure

(use-package cider
  :hook ((clojure-mode . lsp-deferred))
  :general
  (my/normal-state-keys
    :keymaps 'cider-repl-mode-map
    :major-modes t
    "C-M-p" 'cider-repl-previous-input)

  (my/insert-state-keys
    :keymaps 'cider-repl-mode-map
    :major-modes t
    "C-M-p" 'cider-repl-previous-input)

  (my/major-mode-leader-key
    :keymaps 'cider-repl-mode-map
    :major-modes t
    "" '(:ignore t :which-key "CLJ Repl")
    "s" '(cider-repl-handle-shortcut :which-key "shortcuts")
    "q" '(cider-quit :which-key "quit")
    "c" '(cider-repl-clear-buffer :which-key "clear")
    "h" '(:ignore t :which-key "Help/Docs")
    "hr" '(:ignore t :which-key "Resources")
    "hrc" '(clojure-view-cheatsheet :which-key "cheatsheet")
    "hrs" '(clojure-view-style-guide :which-key "styleguide")
    "hd" '(cider-doc :which-key "doc")
    "hc" '(cider-clojuredocs :which-key "clojure doc"))

  (my/lsp-keybindings
   'clojure-mode-map
   "Clojure"
   "'" 'cider-jack-in-clj
   "\"" 'cider-jack-in-cljs
   "hr" '(:ignore t :which-key "Resources")
   "hrc" '(clojure-view-cheatsheet :which-key "cheatsheet")
   "hrs" '(clojure-view-style-guide :which-key "styleguide")
   "hd" '(cider-doc :which-key "doc")
   "hc" '(cider-clojuredocs :which-key "clojure doc")
   "e" '(:ignore t :which-key "Repl")
   "eq" '(cider-quit :which-key "quit")
   "er" '(cider-insert-region-in-repl :which-key "send region")
   "ef" '(cider-insert-defun-in-repl :which-key "send defun")
   "ee" '(:ignore t :which-key "Eval")
   "eef" '(cider-eval-defun-at-point :which-key "function")
   "eer" '(cider-eval-region :which-key "region")
   "eeb" '(cider-eval-buffer :which-key "buffer")
   ;; unbind unsupported operations
   "d" nil
   "Ts" nil
   "Ro" nil
   "wc" nil))

;;;** rust

;; to install the rust language server:
;; $ rustup component add rls rust-analysis rust-src

(use-package rust-mode
  :hook ((rust-mode . lsp))
  :config (my/lsp-keybindings
	   'rust-mode-map "Rust"
	   ;; unbind unsupported operations
	   "=r" nil))

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)))

;;;** dart & flutter

;; In the case that 'flutter doctor' states that the Android licenses were not being accepted
;; (e.g. after a flutter upgrade) proceed with the following:
;; flutter upgrade
;; sudo update-alternatives --config java
;; (switch to java version 8 - Android (and therefore flutter) only works with Java version 8)
;; flutter doctor --android-licenses

;; if you get an error about a JS not finding a specific module navigate to the
;; '.emacs.d/.extension/github/Dart-Code.Dart-Code/extension/' folder and execute 'npm install'

(use-package lsp-dart
  :hook ((dart-mode . lsp-deferred))
  :custom
  (lsp-dart-project-sdk-dir (my//get-value-from-config "dart-sdk" ""))
  (lsp-dart-suggest-from-unimported-libraries nil)
  :init
  (my/lsp-keybindings 'dart-mode-map "Dart"
   ;; unding unsupported functions
   "=r" nil
   "wc" nil)
  :config
  (my//copy-shell-env-variables) ; we need node/npm set up for debugging
  (dap-dart-setup))              ; setup dap debugger
  
(use-package flutter
  :after lsp-dart
  :custom (flutter-sdk-path (my//get-value-from-config "flutter-sdk" ""))
  :general
  (my/major-mode-leader-key
    :keymaps 'dart-mode-map
    :major-modes t
    "R" '(flutter-run-or-hot-reload :which-key "flutter start/reload")))

;;;* outline

;; organize source code in an org like manner
(use-package outline
  :ensure nil
  :defer t
  :config
  (defhydra hydra-outline-movement (:hint nil)
    "
^Movement^              ^Show/Hide^        ^Other^
^^^^^^---------------------------------------------------------
_j_: next visible       _S_: show all      _TAB_: fold cycle
_k_: previous visible   _H_: hide all      _z_: center
                      ^^_O_: hide others
^^^^^^---------------------------------------------------------
[_q_]: quit
^^^^^^---------------------------------------------------------
"
    ("j" outline-next-visible-heading)
    ("k" outline-previous-visible-heading)
    ("S" outline-show-all)
    ("H" (lambda ()
	     (interactive)
	     (outline-hide-sublevels 4)))
    ("O" outline-hide-other)
    ("TAB" outline-cycle)
    ("z" evil-scroll-line-to-center)
    ("q" nil))

  (my/leader-key
    :keymaps 'outline-minor-mode-map
    "l" '(hydra-outline-movement/body :which-key "~Outline~")))

;; add some utility functions for 'outline-mode'
(use-package outline-magic
  :defer t
  :config
  (defun my/outline-cycle-heading ()
    (interactive)
    (when (outline-on-heading-p)
      (outline-cycle)))
  :general
  (my/normal-state-keys
    :keymaps 'outline-minor-mode-map
    "TAB" 'my/outline-cycle-heading))

;; add outline faces for the 'outline-minor-mode'
(use-package outline-minor-faces
  :after outline
  :custom-face (outline-minor-0 ((t (:extend nil))))
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

;;;* misc

;;;** emacs internal

;; add some useful keybindings for the emacs package menu
(use-package package
  :ensure nil
  :config
  (my/leader-key
    :infix my/infix/custom
    "p" '(list-packages :which-key "package menu"))
  (my/major-mode-leader-key
    :keymaps 'package-menu-mode-map
    :major-modes t
    "" '(:ignore t :which-key "Package Menu")
    "f" '(package-menu-filter-by-name :which-key "filter by name")
    "c" '(package-menu-clear-filter :which-key "clear filters")))

;; disable scroll bars (especially on new frames)
(use-package scroll-bar
  :ensure nil
  :custom
  (scroll-bar-mode nil)
  (horizontal-scroll-bar-mode nil))

;; display line numbers in buffers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode))
  :custom
  ;; don't shrink available space to prevent "flickering"
  (display-line-numbers-grow-only t)
  :general
  (my/leader-key
    :infix my/infix/toggle
    "n" '(display-line-numbers-mode :which-key "line numbers")))

;;;** external packages

;; restart emacs from within emacs
(use-package restart-emacs
  :general
  (my/leader-key
    :infix my/infix/quit
    "r" '(restart-emacs :which-key "restart")))

;; edit an (already) opened file as "sudo"
(use-package sudo-edit
  :after recentf
  :defines recentf-exclude
  :init
  ;; exclude sudo opened file from the recentf list
  ;; otherwise we would always be prompted for our password
  (add-to-list 'recentf-exclude "/sudo.*")
  :general
  (my/leader-key
    :infix my/infix/files
    "es" '(sudo-edit :which-key "sudo")))

;; quickly jump to symbol occurences in "spacemacs style"
(use-package highlight-symbol
  :after hydra
  :general
  (my/normal-state-keys "*" 'hydra-symbol/body)
  :config
  (defhydra hydra-symbol (:hint nil
			  :body-pre
			  (progn
			    (highlight-symbol)        ; activate symbol highlighting
			    (global-hl-line-mode -1)) ; deactivate highlighting of the current line
			  :before-exit
			  (progn
			    (highlight-symbol-remove-all) ; remove symbol highlighting
			    (global-hl-line-mode)))       ; re-activate highlighting of the current line
    "
^Navigation^   ^Display^    ^Edit^
^^^^^^-----------------------------------------
_n_: next      _z_: center  _r_: query replace
_N_: previous
^^^^^^-----------------------------------------
[_q_]: quit
^^^^^^-----------------------------------------
"
    ("n" highlight-symbol-next)
    ("N" highlight-symbol-prev)
    ("z" evil-scroll-line-to-center)
    ("r" highlight-symbol-query-replace :exit t)
    ("q" nil)))

;; highlight "todo" (and other) keywords
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; the garbage collection magic hack to only gc when you are idle
(use-package gcmh
  :init (gcmh-mode 1))

;;;** utility functions

(defun java/run-tomcat ()
  "Start your local Tomcat instance.
Read the used 'catalina-path' from the 'config' file (inside your .emacs dir)."
  (interactive)
  (let ((catalina-path (my//get-value-from-config "catalina-path")))
    (comint-send-string
     (get-buffer-process (shell "*tomcat - run*"))
     (concat catalina-path "/bin/catalina.sh run\n"))
    (evil-force-normal-state) ; since we usually just want to watch the logs we switch back to normal state
    (goto-char (point-max)))) ; moves to the end of the buffer in order to see the most recent logs

(defun java/debug-tomcat ()
  "Start a local Tomcat instance in debug mode.
Read the used 'catalina-path' from the 'config' file (inside your .emacs dir)
and read the debug port through user input. Once started we can connect
to the instance via dap-debug choosing 'Java Attach'."
  (interactive)
  (let ((catalina-path (my//get-value-from-config "catalina-path"))
	(port (read-string "Tomcat Port: ")))
    (comint-send-string
     (get-buffer-process (shell "*tomcat - debug*"))
     (concat "export JPDA_ADDRESS=" port " && "
             "export JPDA_TRANSPORT=dt_socket && "
             catalina-path "/bin/catalina.sh jpda run\n"))
    (evil-force-normal-state) ; since we usually just want to watch the logs we switch back to normal state
    (goto-char (point-max)))) ; moves to the end of the buffer in order to see the most recent logs

(my/leader-key
  :infix my/infix/custom
  "t" '(:ignore t :which-key "Tomcat")
  "tt" '(java/run-tomcat :which-key "run")
  "td" '(java/debug-tomcat :which-key "debug"))

(defun remove-all-text-properties (s)
  "Remove all text properties from S."
  (set-text-properties 0 (length s) nil s)
  s)

;;;* the end

;;;** currently disabled packages

(use-package doom-themes
  :disabled t
  :custom
  (doom-themes-enable-bold t)        ; enable bold faces
  (doom-themes-enable-italic t)      ; enable italic faces
  (doom-themes-neotree-file-icons t) ; use the colorful neotree icon theme
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :disabled t
  :custom
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-vcs-max-length 12)
  :init (doom-modeline-mode 1))

(use-package neotree
  :disabled t
  :custom
  ;; jump to current file when opening neotree
  (neo-smart-open t)
  (neo-window-fixed-size nil)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  :general
  (my/leader-key
    "n" '(neotree-toggle :which-key "neotree"))
  :config
  (defun my/neotree-set-default-width ()
    "Set the neotree window width to 25 (default value)."
    (interactive)
    (setq neo-window-width 25)
    (neo-global--reset-width))

  (my/normal-state-keys
    :keymaps 'neotree-mode-map
    "-" '(my/neotree-set-default-width :which-key "minimize"))

  ;; do not resize the neotree window after toggeling
  ;; [src: https://github.com/jaypei/emacs-neotree/issues/262#issuecomment-383352799]
  (add-to-list 'window-size-change-functions
                  (lambda (_)
                    (let ((neo-window (neo-global--get-window)))
                      (unless (null neo-window)
                        (setq neo-window-width (window-width neo-window)))))))

;;;** local variables & file end

;; mark 'outline-hide-sublevels' as a safe local variable
(custom-set-variables
 '(safe-local-variable-values '((eval outline-hide-sublevels 4))))

;; Local Variables:
;; outline-regexp: ";;;\\*+"
;; eval: (outline-minor-mode 1)
;; eval: (outline-hide-sublevels 4)
;; End:

(provide 'init)

;;; init.el ends here
