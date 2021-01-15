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

;;;* setup

;;;** startup speed

;; keep the startup time of Emacs low by applying the following techniques:
;; - use "lexical-binding" (see first line of this file)
;; - avoid garbage collection during startup
;; - unset `file-name-handler-alist' temporarily
;; - set `frame-inhibit-implied-resize' to t
;;
;; for more information have a look at the following links:
;; - https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;; - https://nullprogram.com/blog/2017/01/30/
;; - https://old.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/

;; save file name handler reference for the reset after startup
(defvar my--file-name-handler-alist file-name-handler-alist)

(setq ;; unset `file-name-handler-alist' temporarily
      file-name-handler-alist nil
      ;; turning up garbage collection threshold and percentage temporarily
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; prevent emacs from resizing the (GUI) frame when your newly set font has a different size from the system default
      frame-inhibit-implied-resize t)

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
     ;; reset `file-name-handler-alist' to avoid complications
     file-name-handler-alist my--file-name-handler-alist)))

;;;** fonts

;; set custom fonts for the emacs GUI application
;; the fonts are configured by setting some emacs standard faces for text appearance

;; | face             | description                                          |
;; |------------------|------------------------------------------------------|
;; | `default'        | main typeface for text that doesn't specify any face |
;; | `fixed-pitch'    | face that forces the use of a fixed width font       |
;; | `variable-pitch' | face that forces the use of a variable-width-font    |

;; NOTE if one of the following fonts is not installed, a warning will be shown on startup
(when (display-graphic-p)
  (let ((fixed-width-font    "Source Code Pro")
        (variable-width-font "Alegreya Sans")
        (fallback-font       "Noto Sans")
        (error-msg "The font '%s' is not installed on your system. Install it to ensure a proper configuration."))

    ;; set a monospaced font for the `default' and `fixed-pitch' faces to ensure a correct alignment of text
    (if (find-font (font-spec :name fixed-width-font))
      (progn
        (set-face-attribute 'default nil :font fixed-width-font :height 110 :weight 'normal)
        (set-face-attribute 'fixed-pitch nil :font fixed-width-font :height 110 :weight 'normal))
      (warn (format error-msg fixed-width-font)))

    ;; set a proportionately spaced font for the `varible-pitch' face for a better readability (e.g. in `org-mode')
    (if (find-font (font-spec :name variable-width-font))
      (set-face-attribute 'variable-pitch nil :font variable-width-font :height 140 :weight 'semi-light)
      (warn (format error-msg variable-width-font)))

    ;; a fallback fontset is defined in the case a font does not provide certain glyphs
    ;; for a detailed explanation check the following link:
    ;; - https://idiocy.org/emacs-fonts-and-fontsets.html
    (if (find-font (font-spec :name fallback-font))
      (set-fontset-font t 'latin fallback-font)
      (warn (format error-msg fallback-font)))))

;;;** use-package

;; `use-package' is not a package manager but a macro to isolate package configuration
;; it helps to keep the configuration tidy and performance oriented (lazy loading)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; prevent double initialization
(setq package-enable-at-startup nil)
(unless (bound-and-true-p package--initialized)
   (package-initialize))

;; ensure that the `use-package' package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq
 use-package-always-ensure t       ; ensure all packages added via `use-package'
 use-package-compute-statistics t) ; enable this to see package loading statistics

;;;** libraries

;; to ensure everything is working fine you have to install the fonts
;; necessary by running the command (M-x) `all-the-icons-install-fonts'
(use-package all-the-icons)

;; utility libraries to make emacs lisp a more viable programming language
(use-package s)               ; string manipulation
(use-package dash)            ; modern list api
(use-package dash-functional) ; function combinators
(use-package f)               ; modern api to work with files & directories

;;;** defaults

;; set some general configuration options to make the emacs experience more enjoyable

(setq
 inhibit-startup-screen t                    ; disable the start-up screen
 initial-scratch-message ""                  ; empty the initial *scratch* buffer
 initial-major-mode 'fundamental-mode        ; set 'fundamental-mode' for scratch buffer
 sentence-end-double-space nil               ; end sentences with just one space (default: two)
 create-lockfiles nil                        ; lockfiles don't provide a lot of benefit
 scroll-conservatively most-positive-fixnum  ; always scroll by one line
 ring-bell-function 'ignore                  ; turn off the bell sound
 x-stretch-cursor t                          ; make cursor the width of the character underneath (e.g. full width of a TAB)
 delete-by-moving-to-trash t                 ; move deleted files to trash instead of deleting them outright
 load-prefer-newer t)                        ; always load the newest version of an elisp file

(when (>= emacs-major-version 28)
  ;; automatically generate the natively compiled files for each new .elc
  (defvar comp-deferred-compilation)
  (setq comp-deferred-compilation t))

(set-language-environment "UTF-8")          ; default to utf-8 encoding
(add-to-list 'default-frame-alist
	     '(fullscreen . maximized))     ; maximize the emacs window on startup

;; answering with 'y' or 'n' is sufficient
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package cus-edit
  :ensure nil
  :custom
  ;; write the customization block to another file (but never load it)
  ;; this prevents the 'init.el' file to getting cluttered with "customization" code
  (custom-file (concat user-emacs-directory "ignore-customizations.el")))

;; fix resizing issues of child frames with GTK3 and GNOME
;; for more information have a look at the following links:
;; - https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-27&id=c49d379f17bcb0ce82604def2eaa04bda00bd5ec
;; - https://github.com/tumashu/company-posframe/issues/2

;; NOTE this option migth be removed in the future
(setq x-gtk-resize-child-frames 'hide)

;; remove not needed GUI elements

(use-package menu-bar
  :ensure nil
  :config (menu-bar-mode -1))

(use-package tool-bar
  :ensure nil
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :ensure nil
  :config
  ;; never display vertical & horizontal scrollbars
  ;; (for the current as well as new frames)
  (set-scroll-bar-mode nil)
  (toggle-horizontal-scroll-bar -1))

(use-package tooltip
  :ensure nil
  :config
  ;; do not show tooltips in pop-up windows (but in the echo area instead)
  (tooltip-mode -1))

;;;** copy & paste

;; emacs and vim do not really differentiate between "copying", "deleting" and "cutting" text
;; if one copies some text A, then deletes text B and then calls a paste operation, the inserted text will be B (by default)
;; this can absolutely f### with peoples heads who are used to a more separated flow of copying, deleting and cutting
;; therefore this section tries to make emacs (with `evil') behave more like a modern text editor

;; vim uses multiple storage spaces for text the so called registers
;; every operation can be called with a specific register (using the " prefix key)
;; e.g. '"3yw' copies the next word into register '3', '"ap' pastes the text from register 'a'

;; there are some special registers:
;; | register | content                                                |
;; |----------|--------------------------------------------------------|
;; | 0        | last copied text                                       |
;; | "        | last copied OR deleted text (default source for paste) |
;; | *        | primary selection                                      |
;; | +        | desktop clipboard                                      |
;; | ...      | ...                                                    |

(defvar my//evil-yank-default-register ?0
  "By default `evil-yank' automatically writes into the 0 register.
By changing this variable one changes the default target for all evil 'yank' commands.
Furthermore set the default source for pasting to this register, so that deleted texts will be ignored by it.")

;; if emacs was started as a GUI application or if "xclip" is installed
;; the default yank/paste register is changed to '+' (the systems clipboard)
(when (or (display-graphic-p)
	  (executable-find "xclip"))
  (setq select-enable-clipboard nil) ; delete/cut operations will not write to the clipboard
  (setq my//evil-yank-default-register ?+))

(when (not (display-graphic-p))
  (if (executable-find "xclip")
      ;; make sure that xclip is used for all copy operations in terminal emacs
      (use-package xclip
	:config
	(xclip-mode 1))
    (message "For access to the system clipboard (from the terminal) please install 'xclip'")))

(with-eval-after-load "evil"

  ;; we advise all relevant "yank" and "paste" functions to use `my//evil-yank-default-register'

  (defun my//evil-yank-advice (orig-fn beg end &optional type register yank-handler)
    "Advice function for `evil-yank' which will set the default REGISTER to `my//evil-yank-default-register'.
Otherwise this calls ORIG-FN and pass BEG, END, TYPE and YANK-HANDLER to it."
    (defvar evil-was-yanked-without-register) ; to surpress compiler warnings
    (when (and evil-was-yanked-without-register (not register))
      (setq register my//evil-yank-default-register))
    (apply orig-fn beg end type register (list yank-handler)))
  (advice-add 'evil-yank :around 'my//evil-yank-advice)

  (defun my//evil-yank-line-advice (orig-fn beg end &optional type register)
    "Advice function for `evil-yank-line' which will set the default REGISTER to `my//evil-yank-default-register'.
Otherwise this calls ORIG-FN and pass BEG, END, TYPE and YANK-HANDLER to it."
    (defvar evil-was-yanked-without-register) ; to surpress compiler warnings
    (when (and evil-was-yanked-without-register (not register))
      (setq register my//evil-yank-default-register))
    (apply orig-fn beg end type (list register)))
  (advice-add 'evil-yank-line :around 'my//evil-yank-line-advice)

  (defun my//evil-paste-advice (orig-fn count &optional register yank-handler)
    "Advice function for `evil-paste-after' and `evil-paste-before' to use the default REGISTER `my//evil-yank-default-register'.
Otherwise this calls ORIG-FN and passes COUNT, REGISTER and YANK-HANDLER to it."
    (when (not register)
      (setq register my//evil-yank-default-register))
    (apply orig-fn count register (list yank-handler)))
  (advice-add 'evil-paste-after :around 'my//evil-paste-advice)
  (advice-add 'evil-paste-before :around 'my//evil-paste-advice)

  ;; use "x" in visual state for cutting (copy followed by delete)
  ;; instead of being just another keybinding for delete

  (defun my/evil-cut ()
    "Copy the selected region, then delete it."
    (interactive)
    (evil-yank (region-beginning) (region-end))
    (evil-delete-char (region-beginning) (region-end))
    (evil-force-normal-state))

  (defvar evil-visual-state-map)
  (define-key evil-visual-state-map "x" 'my/evil-cut))

(with-eval-after-load "evil-org"
  ;; special case for `evil-org' as some functions yank the text to a before deleting it
  ;; prevent these calls to `evil-yank' so they do not clutter `my//evil-yank-default-register' with deleted text

  (defun my//evil-org-delete-char-advice (_orig-fn count &rest _rest)
    "Ignore all the custom logic of `evil-org-delete-char'.
Instead call `org-delete-char' with COUNT and ignore the REST."
    (org-delete-char count))
  (advice-add 'evil-org-delete-char :around 'my//evil-org-delete-char-advice)

  (defun my//evil-org-delete-backward-char-advice (_orig-fn count &rest _rest)
    "Ignore all the custom logic of `evil-org-delete-backward-char'.
Instead call `org-delete-backward-char' with COUNT and ignore the REST."
    (org-delete-backward-char count))
  (advice-add 'evil-org-delete-backward-char :around 'my//evil-org-delete-backward-char-advice))

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

;;;* keybindings

;; display available keybindings
(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-max-description-length 30)
  :config (which-key-mode 1))

;; more convenient key definitions in emacs
(use-package general
  :init
  ;; upper case top level prefixes
  (defconst my/infix/frames "F")
  (defconst my/infix/toggle "T")
  ;; lower case top level prefixes
  (defconst my/infix/buffer "b")
  (defconst my/infix/custom "c")
  (defconst my/infix/dired "d")
  (defconst my/infix/files "f")
  (defconst my/infix/git "g")
  (defconst my/infix/help "h")
  (defconst my/infix/insert "i")
  (defconst my/infix/jump "j")
  (defconst my/infix/org "o")
  (defconst my/infix/projects "p")
  (defconst my/infix/quit "q")
  (defconst my/infix/search "s")
  (defconst my/infix/tabs "t")
  (defconst my/infix/windows "w")
  (defconst my/infix/spellcheck "k")
  :config
  ;; declare general definers for the leader
  (general-create-definer my/leader-key
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  ;; unbind "SPC" to prevent conflicts with other keybindings
  (my/leader-key "" nil)

  ;; declare general definers for the local leader (current major mode)
  (general-create-definer my/major-mode-leader-key
    :states '(normal visual motion emacs insert)
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  ;; set top level prefix `which-key' description
  (mapc (lambda (x)
	   (let ((prefix-key (-first-item x))
		 (prefix-desc (-second-item x)))
	   (eval `(my/leader-key
		    ,prefix-key '(:ignore t :which-key ,prefix-desc)))))
	`((,my/infix/frames "Frames")
	  (,my/infix/toggle "Toggles")
	  (,my/infix/buffer "Buffer")
	  (,my/infix/custom "Custom")
	  (,my/infix/dired "Dired")
	  (,my/infix/files "Files")
	  (,my/infix/git "Git")
	  (,my/infix/help "Help")
	  (,my/infix/insert "Insert")
	  (,my/infix/jump "Jump")
	  (,my/infix/org "Org")
	  (,my/infix/projects "Projects")
	  (,my/infix/quit "Quit")
	  (,my/infix/search "Search")
	  (,my/infix/tabs "Tabs")
	  (,my/infix/windows "Windows")
	  (,my/infix/spellcheck "Spellcheck")))

  ;;
  ;; declare definers for different `evil' states
  ;;

  (general-create-definer my/all-states-keys
    :states '(normal visual motion emacs insert))
  (general-create-definer my/normal-state-keys
    :states '(normal motion))
  (general-create-definer my/visual-state-keys
    :states '(visual))
  (general-create-definer my/insert-state-keys
    :states '(insert)))

;; make emacs bindings that stick around
(use-package hydra)

;; building commands with prefix keys and arguments
(use-package transient)

;;;* styling

;;;** theme

;; properly configure a "light" and a "dark" color theme for emacs
;; - make it easy to switch between both themes (without the need to touch the configuration)
;; - startup emacs with the appropriate theme depending on the current time

(use-package modus-themes
  :custom
  (modus-themes-headings '((t . rainbow)))
  (modus-themes-org-blocks 'grayscale)
  :hook
  (modus-themes-after-load-theme . (lambda nil
				     (powerline-reset)                ; reset spaceline faces
				     (my//reset-evil-state-cursors))) ; reset custom cursor config
  :config

  (defun my//dark-mode-wk-replacement (entry)
    "Which key replacement function that shows the currently present state."
    (let ((key (car entry)))
      (pcase (car custom-enabled-themes)
	('modus-operandi `(,key . "[ ] dark mode"))
	('modus-vivendi `(,key . "[X] dark mode"))
	(_ `(,key . "custom theme loaded")))))

  ;; configure a keybinding to switch between light & dark theme
  (my/leader-key
    :infix my/infix/toggle
    "D" '(modus-themes-toggle :which-key my//dark-mode-wk-replacement))

  (defun my//load-theme-on-startup ()
    "Checks the current time and loads the appropriate theme (light or dark) for it."
    (let ((hour (string-to-number (substring (current-time-string) 11 13))))
      (if (member hour (number-sequence 7 19))
	(load-theme 'modus-operandi t)
	(load-theme 'modus-vivendi t))))

  ;; load startup theme depending on the current time
  (my//load-theme-on-startup)
  ;; reset spaceline faces
  (with-eval-after-load "powerline"
    (powerline-reset))
  ;; reset custom cursor config
  (with-eval-after-load "evil"
    (my//reset-evil-state-cursors)))

;;;** modeline

;; style and configure the modeline with `spaceline'
;;
;; the following information is displayed (in order of appearance left to right):
;; - `winum' window number
;; - buffer modification status
;; - buffer size
;; - buffer id
;; - cursor position in document
;; - `flycheck' warning/error count
;; - `lsp-mode' status
;; - `dap-mode' status
;; - current vc branch
;; - current major mode

;; base spaceline package
(use-package spaceline
  :custom
  ;; change the highlight face depending on the modification status
  (spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;; predefined spaceline segments
(use-package spaceline-segments
  :ensure spaceline
  :config
  (defun my//flycheck-status ()
    "If `flycheck-mode' is enabled, check for the current status and show an appropriate icon plus the number of warnings/errors (if any are present)."
    (when (bound-and-true-p flycheck-mode)
      ;; declarations to prevent compiler warnings
      (defvar flycheck-last-status-change)
      (defvar flycheck-current-errors)

      (pcase flycheck-last-status-change
	(`finished (if-let ((all-errors (flycheck-count-errors flycheck-current-errors)))
		       (let* ((warnings
			       ;; check for entries that contain the keyword "warning", then sum up all the occurrences
			       ;; (e.g. "lsp-flycheck-warning-unnecessary", "lsp-flycheck-warning-deprecated")
			       (-sum (-map 'cdr (-filter (-compose (-partial 's-contains? "warning")
								   'symbol-name
								   '-first-item) all-errors))))
			      (warnings-info (when (> warnings 0)
					       (concat (propertize (all-the-icons-faicon "exclamation-circle" :v-adjust 0.05)
								   'face '((:family "FontAwesome" :foreground "dark orange")))
						       ;; insert a tiny bit of space between the warning icon and count
						       (propertize " " 'face '(:height 0.2 :inherit))
						       (propertize (format "%s" warnings)
								   'face '((:foreground "dark orange" :weight bold))))))
			      (errors (assq 'error all-errors))
			      (errors-info (when errors
					     (concat (propertize (all-the-icons-faicon "ban" :v-adjust 0.05)
								 'face '((:family "FontAwesome" :foreground "red")))
						     ;; insert a tiny bit of space between the error icon and count
						     (propertize " " 'face '(:height 0.2 :inherit))
						     (propertize (format "%s" (cdr errors))
								 'face '((:foreground "red" :weight bold)))))))
			 (s-join " " (-non-nil (list warnings-info errors-info))))
		     (propertize (all-the-icons-faicon "check-circle" :v-adjust -0.1)
				 'face '((:family "FontAwesome" :foreground "dark green")))))
	(`running (propertize (all-the-icons-faicon "spinner" :v-adjust -0.1)
			      'face '((:family "FontAwesome" :foreground "#29aeff"))))
	(`no-checker "")
	(`not-checked (propertize (all-the-icons-faicon "frown-o" :v-adjust -0.1)
				  'face '((:family "FontAwesome"))))
	(`errored     (propertize (all-the-icons-faicon "exclamation" :v-adjust -0.1)
				  'face '((:family "FontAwesome"))))
	(`interrupted (propertize (all-the-icons-faicon "plug" :v-adjust -0.1)
				  'face '((:family "FontAwesome"))))
	(`suspicious  (propertize (all-the-icons-faicon "bug" :v-adjust -0.1)
				  'face '((:family "FontAwesome")))))))

  ;; custom spaceline segments
  (spaceline-define-segment my//flycheck
    "Displays `flycheck' errors and warnings"
    (my//flycheck-status))

  (spaceline-define-segment my//vc-branch
    "Displays the current vc/git branch"
    (when vc-mode
      (concat
       ;; use `substring' to strip the "Git: " prefix from the branch name
       (s-truncate 45 (substring vc-mode 5))
       " "
       (propertize (all-the-icons-octicon "git-branch" :v-adjust -0.1)
		   'face '((:family "github-octicons"))
		   ;; show complete branch name on mouse hover over the icon
		   'help-echo vc-mode))))

  (spaceline-define-segment my//lsp-status-icon
    "Icon showing the current connection status of `lsp-mode'"
    (when (bound-and-true-p lsp-mode)
      (if (lsp-workspaces)
	(propertize (all-the-icons-faicon "rocket" :v-adjust -0.05)
		    'face '((:family "FontAwesome" :foreground "#44bc44")))
	(propertize (all-the-icons-faicon "rocket" :v-adjust -0.05)
		    'face '((:family "FontAwesome" :foreground "red"))))))

  (spaceline-define-segment my//lsp-info
    "Displays the `lsp-mode-line' info string"
    (when (bound-and-true-p lsp-mode)
      (concat "["
	      (mapconcat #'lsp--workspace-print
			 lsp--buffer-workspaces
			 "][")
	      "]")))

  (spaceline-define-segment my//dap-info
    "Icon indicating a present `dap-mode' session"
    (when (and (bound-and-true-p lsp-mode)
	       (bound-and-true-p dap-mode))
      (when-let ((session (dap--cur-session)))
	(when (dap--session-running session)
	  (propertize (all-the-icons-faicon "bug" :v-adjust -0.1)
		      'face '((:family "FontAwesome" :foreground "purple")))))))

  (spaceline-compile
    ;; left side
    '(((window-number buffer-modified buffer-size)
       :face highlight-face
       :priority 95)
      ((buffer-id remote-host) :priority 100)
      (buffer-position :when active
		       :priority 85)
      (my//flycheck :when active
		    :priority 70)
      ((my//lsp-status-icon my//dap-info)
       :when active
       :skip-alternate t
       :priority 60)
      (my//lsp-info :when active
		    :priority 30))
    ;; right side
    '((my//vc-branch :when active
		     :priority 80)
      (major-mode :priority 90))))

;;;* evil

;; an extensible vi layer which emulates the main features of vim and turns emacs into a modal editor
(use-package evil
  :init
  ;; we have to define this function before the package being loaded
  ;; so it works with '(with-eval-after-load "evil" ...)' blocks
  (defun my//reset-evil-state-cursors ()
    "Reset shape and color of all evil state cursors."
    (let ((cursor-color (face-background 'cursor)))
      (setq
       ;; style `evil-emacs-state-cursor' to differentiate easily from the other states
       evil-emacs-state-cursor '("purple" (bar . 3))
       ;; set all other state cursors to their default shape and color defined by the current theme
       evil-normal-state-cursor   `(,cursor-color box)
       evil-visual-state-cursor   `(,cursor-color box)
       evil-insert-state-cursor   `(,cursor-color (bar . 2))
       evil-operator-state-cursor `(,cursor-color evil-half-cursor)
       evil-motion-state-cursor   `(,cursor-color box)
       evil-replace-state-cursor  `(,cursor-color hbar))))
  :custom
  (evil-want-minibuffer t)        ; integrate `evil' into the minibuffer
  (evil-want-C-u-scroll t)        ; scroll up with 'C-u' like in vim
  (evil-want-Y-yank-to-eol t)     ; 'Y' should yank to the end of the line (instead of the whole line)
  (evil-kill-on-visual-paste nil) ; pasting in visual state should NOT add the replaced text to the kill ring
  (evil-split-window-below nil)   ; split windows are created above
  (evil-vsplit-window-right nil)  ; vertically split windows are created on the left
  ;; these two options are set as required by `evil-collection'
  (evil-want-integration t)       ; load 'evil-integration.el' (default)
  (evil-want-keybinding nil)      ; do NOT load 'evil-keybindings.el' as `evil-collection' is used instead
  (evil-want-C-i-jump nil)        ; remove C-i/TAB jump binding from vim (see: https://github.com/Somelauw/evil-org-mode#common-issues)

  ;; use `undo-fu' as the "evil undo system" (since `undo-tree' is known for its errors)
  ;; starting with emacs 28 there is also the already included `undo-redo' function which could be used instead
  ;; to ensure backwards compatibility with versions prior to 28 use the `undo-fu' package for now
  ;; for more information check the issue in the `evil' repository:
  ;; - https://github.com/emacs-evil/evil/issues/1074
  (evil-undo-system 'undo-fu)
  :config
  ;; setup the most generic minibuffer keybindings for `evil'
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'minibuffer-local-map
   "C-g" 'minibuffer-keyboard-quit
   "RET" 'exit-minibuffer)

  ;; `evil-set-initial-state' can not be used since the minibuffer has no mode
  (add-hook 'minibuffer-setup-hook 'evil-insert-state)

  ;; just some general useful insert state keybindings
  (general-def 'insert
    ;; "restore" some emacs bindings
    "C-a" 'evil-beginning-of-line
    "C-e" 'evil-end-of-visual-line
    "C-d" 'delete-char
    ;; utility insert state bindings
    "M-o" 'evil-open-below
    "M-O" 'evil-open-above)

  ;;
  ;; utility functions to insert newlines below or above the current point
  ;; these ones are shamelessly "borrowed" from spacemacs
  ;;
  
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
  
  ;;
  ;; utility functions for window splitting (and focus) and window related keybindings
  ;;

  (defun my/evil-vsplit-right-and-focus ()
    "Split the current window vertically and focus the new window on the right."
    (interactive)
    (let ((evil-vsplit-window-right t))
      (evil-window-vsplit)))

  (defun my/evil-split-below-and-focus ()
    "Split the current window horizontally and focus the new window below."
    (interactive)
    (let ((evil-split-window-below t))
      (evil-window-split)))
  
  (my/leader-key
    :infix my/infix/windows
    "V" '(my/evil-vsplit-right-and-focus :which-key "split → and focus")
    "S" '(my/evil-split-below-and-focus :which-key "split ↓ and focus")
    "v" '(evil-window-vsplit :which-key "split →")
    "s" '(evil-window-split :which-key "split ↓")
    "h" '(evil-window-left :which-key "go left")
    "l" '(evil-window-right :which-key "go right")
    "j" '(evil-window-down :which-key "go down")
    "k" '(evil-window-up :which-key "go up")
    "d" '(:ignore t :which-key "Delete Window")
    "dd" '(evil-window-delete :which-key "current"))

  (evil-mode))

;; evil bindings for the parts of emacs that are not covered properly by default
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil) ; deactivate `company-tng'
  :config
  ;; enable bindings only for desired modes in order to keep control
  (evil-collection-init '(company
			  comint
			  compile
			  dired
			  magit
			  (package-menu package)
			  (term term ansi-term multi-term)))

  ;; enable `evil-scroll-down' also in visual state
  (my/visual-state-keys
    :keymaps 'comint-mode-map
    "C-d" 'evil-scroll-down)

  (my/insert-state-keys
    :keymaps 'comint-mode-map
    ;; enable default binding for previous-input
    "C-p" 'comint-previous-input)

  ;; unbind `describe-mode' to prevent conflict with evil navigation
  (general-unbind compilation-mode-map "h")

  ;; add help shortcut for packages
  (my/normal-state-keys
    :keymaps 'package-menu-mode-map
    "?" 'package-menu-describe-package))

;; custom key sequence to "escape" from insert state (and everything else)
(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "fd")
  (evil-escape-delay 0.2)
  :config
  (evil-escape-mode))

;; provides 2-character motions for quickly jumping around text
(use-package evil-snipe
  :after evil
  :custom
  ;; BUG in the case of `evil-snipe-skip-leading-whitespace' being non-nil
  ;; `evil-snipe-t'/`evil-snipe-T' will snipe onto whitespace characters instead of before them
  ;; (when sniping for whitespaces , e.g. t<space> or T<space>)
  (evil-snipe-skip-leading-whitespace nil)
  :custom-face
  (evil-snipe-matches-face ((t (:background "#00bdfa" :inherit default))))
  :config
  (evil-snipe-mode 1)           ; enable evil snipe mode
  (evil-snipe-override-mode 1)) ; enable alternate behaviour for "f/t/F/T" keys

;; delete, change and add surrounding in pairs
(use-package evil-surround
  :after (evil evil-snipe)
  :general
  ;; bind manually instead of using `global-evil-surround-mode'
  ;; to prevent conflicts with `evil-snipe' keybindings
  (my/visual-state-keys
    "gs" 'evil-surround-region
    "gS" 'evil-Surround-region)

  (general-define-key
   :states '(operator)
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)

  :config
  ;; swap `evil-surround' default behavior:
  ;; - use non-spaced pairs when surrounding with an opening brace
  ;; - use spaced pairs when surrounding with a closing brace
  (evil--add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

;; comment stuff out
(use-package evil-commentary
  :after evil
  :general
  (general-define-key
   :states '(normal)
   "gc" 'evil-commentary
   "gy" 'evil-commentary-yank))

;; multiple cursors for `evil', based on iedit
(use-package evil-multiedit
  :after evil
  :general
  (general-define-key
   :states '(normal visual)
   "M-d" 'evil-multiedit-match-and-next
   "M-D" 'evil-multiedit-match-and-prev)

  (general-define-key
   :states '(visual)
   "R" 'evil-multiedit-match-all)

  :config
  (general-define-key
   :keymaps 'evil-multiedit-state-map
   "M-d" 'evil-multiedit-match-and-next
   "M-D" 'evil-multiedit-match-and-prev
   "RET" 'evil-multiedit-toggle-or-restrict-region)

  (general-define-key
   :keymaps '(evil-multiedit-state-map
	      evil-multiedit-insert-state-map)
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev))

;; `evil-disable-mouse' is a local package and needs to be installed manually
(when (not (package-installed-p 'evil-disable-mouse))
  (package-install-file (concat user-emacs-directory "repos/evil-disable-mouse/")))

(use-package evil-disable-mouse
  :ensure nil
  :init
  (defun my//evil-disable-mouse-wk-replacement (entry)
    "Which key replacement function for `evil-disable-mouse-mode' that checks if the mode is currently active."
    (let ((key (car entry)))
      (if (bound-and-true-p evil-disable-mouse-global-mode)
	`(,key . "[ ] mouse support")
	`(,key . "[X] mouse support"))))
  :general
  (my/leader-key
    :infix my/infix/toggle
    "M" '(evil-disable-mouse-global-mode :which-key my//evil-disable-mouse-wk-replacement)))

;;;* files

;; file related operations & functions
(use-package files
  :ensure nil
  :preface
  (defconst my-backup-directory (concat user-emacs-directory "backups"))
  ;; create the backup folder if it does not exist yet
  (when (not (file-exists-p my-backup-directory))
    (make-directory my-backup-directory t))
  :init
  (defun my//insert-file-path-wk-replacement (entry)
    "Whick key replacement function for `my/insert-file-path' to indicate if a prefix argument is currently present."
    (let ((key (car entry)))
      (if prefix-arg
	`(,key . "file path (rel)")
	`(,key . "file path (abs)"))))
  :custom
  ;; save all backup files to a backup folder inside the emacs directory
  (backup-directory-alist `(("." . ,my-backup-directory)))
  (make-backup-files t)   ; backup a file the first time it is saved
  (backup-by-copying t)   ; don't get problems with symlinks
  (version-control t)     ; version numbers for backup files
  (delete-old-versions t) ; delete excess backup files silently
  (kept-old-versions 6)   ; oldest version to keep when a new numbered backup is made (default: 2)
  (kept-new-versions 9)   ; newest version to keep when a new numbered backup is made (default: 2)
  (auto-save-default t)   ; auto-save every buffer that visits a file
  :general
  (my/leader-key
    :infix my/infix/files
    "s" '(save-buffer :which-key "save file")
    "e" '(:ignore t :which-key "Edit")
    "ec" '(my/copy-current-file :which-key "copy")
    "er" '(my/rename-current-file-and-buffer :which-key "rename")
    "ed" '(my/delete-current-file-and-buffer :which-key "delete"))

  (my/leader-key
    :infix my/infix/buffer
    "r" '(my/revert-buffer :which-key "revert"))

  (my/leader-key
    :infix my/infix/insert
    "f" '(my/insert-file-path :which-key my//insert-file-path-wk-replacement))

  (my/leader-key
    :infix my/infix/quit
    "q" '(save-buffers-kill-terminal :which-key "quit"))
  :config
  (defun my/revert-buffer ()
    "Synchronize the current buffer's state with its corresponding file."
    (interactive)
    (revert-buffer t (not (buffer-modified-p)) t))

  (defun my/copy-current-file ()
    "Copy the current file.
Throws a `user-error' if the current buffer does not visit a file."
    (interactive)
    (if buffer-file-name
      (let ((new-file (read-file-name (format "Copy %s to file: " buffer-file-name))))
	(copy-file buffer-file-name new-file 1))
      (user-error "Current buffer does not visit a file")))

  (defun my/rename-current-file-and-buffer ()
    "Rename the current file.
Throws a `user-error' if the current buffer does not visit a file or if the file does not exist."
    (interactive)
    (if buffer-file-name
	(if (file-exists-p buffer-file-name)
	  (let ((new-name (read-file-name (format "Rename %s to: " buffer-file-name))))
	    (cond
	     ((vc-backend buffer-file-name) (vc-rename-file buffer-file-name new-name))
	     (t (rename-file buffer-file-name new-name 1)
		(set-visited-file-name new-name t t))))
	   (user-error "File does not exist"))
	(user-error "Current buffer does not visit a file")))

  (defun my/delete-current-file-and-buffer ()
    "Delete the current file.
Throws a `user-error' if the current buffer does not visit a file."
    (interactive)
    (if buffer-file-name
	(when (y-or-n-p (format "Do you really want to delete %s? " buffer-file-name))
	  (if (vc-backend buffer-file-name)
	      (vc-delete-file buffer-file-name)
	    (progn
	      (delete-file buffer-file-name)
	      (message "Deleted file %s" buffer-file-name)
	      (kill-buffer))))
      (user-error "Current buffer does not visit a file")))

  (defun my/insert-file-path (filename &optional arg)
    "Inserts the absolute path to FILENAME.

If called with any prefix ARG this will instead insert the relative path to FILENAME.
In this case the source of the relative path is the current buffer or file."
    (interactive "*fInsert file name: \nP")
    (insert (if arg
	      (file-relative-name filename)
	      filename))))

;; re-open recently opened files
(use-package recentf
  :ensure nil
  :custom
  ;; increase number of saved recent files (default: 20)
  (recentf-max-saved-items 50))

;; edit an (already) opened file as "sudo"
(use-package sudo-edit
  :after recentf
  :defines recentf-exclude
  :init
  ;; exclude files which were opened with root from the recent files
  ;; otherwise every call to `recentf' would prompt for the password
  (add-to-list 'recentf-exclude "/sudo.*")
  :general
  (my/leader-key
    :infix my/infix/files
    "es" '(sudo-edit :which-key "sudo")))

;;;* buffers

;; remap emacs faces per buffer
(use-package face-remap
  :ensure nil
  :general
  (my/leader-key
    :infix my/infix/buffer
    "z" '(hydra-zoom/body :which-key "[zoom level]"))
  :config
  (defun my/default-text-size ()
    "Reset the text size to the default value."
    (interactive)
    (text-scale-set 0))

  ;; hydra to zoom the text inside the current buffer
  (defhydra hydra-zoom ()
    "Text zoom level (current buffer)"
    ("i" text-scale-increase "zoom in")
    ("o" text-scale-decrease "zoom out")
    ("0" my/default-text-size "default")
    ("q" nil "quit")))

;; interactively flip between recently visited buffers
(use-package iflipb
  :custom
  (iflipb-format-buffers-function
   'iflipb-format-buffers-vertically) ; display buffers in a vertical list
  (iflipb-wrap-around t)              ; restart from the beginning when the end of the buffer list is reached
  (iflipb-ignore-buffers "^[:]")      ; only ignore `dired-sidebar' buffers (starting with ":")
  :general
  (my/leader-key "TAB" '(hydra-iflipb/body :which-key "previous buffer"))
  :config
  (defhydra hydra-iflipb (;; call `iflipb-next-buffer' before the hydra opens to directly start a new `iflipb' sequence without any additional key press
			  :body-pre (iflipb-next-buffer nil)
			  ;; `iflipb' checks the `last-command' to see if the next call should move the selection in the current sequence or if a new one should be started
			  ;; to ensure that the hydra does not interfere with this procedure the `last-command' is set manually before each key press
			  :pre (setq last-command 'iflipb-next-buffer))
    ""
    ;; bind <tab> and TAB so `iflipb' does not stop in org buffers
    ("TAB" iflipb-next-buffer nil)
    ("<tab>" iflipb-next-buffer "backwards")
    ("<backtab>" iflipb-previous-buffer "forwards")
    ("q" nil "quit")))

;;;* windows

;; basic configuration for windows
;; NOTE: since 'window.el.gz' does not provide a feature we have to use the emacs pseudo package
(use-package emacs
  :config
  (my/leader-key
    :infix my/infix/windows
    "=" '(hydra-window/body :which-key "[window size]")
    "m" '(delete-other-windows :which-key "maximize")
    "x" '(kill-buffer-and-window :which-key "kill buffer & window"))

  (defun my/shrink-window-vertically ()
    "Shrink the vertical window size."
    (interactive)
    (let ((current-prefix-arg '(-1)))
      (call-interactively 'enlarge-window)))

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

;; window movement functions
(use-package windmove
  :ensure nil
  :config
  (my/leader-key
    :infix my/infix/windows
    ;; delete other windows
    "dh" '(windmove-delete-left :which-key "left")
    "dj" '(windmove-delete-down :which-key "down")
    "dk" '(windmove-delete-up :which-key "up")
    "dl" '(windmove-delete-right :which-key "right")
    "w" '(:ignore t :which-key "Swap Windows")
    "wh" '(windmove-swap-states-left :which-key "left")
    "wj" '(windmove-swap-states-down :which-key "down")
    "wk" '(windmove-swap-states-up :which-key "up")
    "wl" '(windmove-swap-states-right :which-key "right")))


;; record window layout configurations (for undo/redo)
(use-package winner
  :ensure nil
  :config
  (winner-mode) ; start "recording"

  (my/leader-key
    :infix my/infix/windows
    "u" '(winner-undo :which-key "winner undo")))

;; mark windows with numbers for easier navigation
(use-package winum
  :custom
  (winum-scope 'frame-local)
  (winum-auto-setup-mode-line nil) ; prevent modeline auto configuration
  :config
  ;; use the `which-key-replacement-alist' to bundle "winum-select-window-x" functions in `which-key' to a single entry
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
	which-key-replacement-alist)
  (push '((nil . "winum-select-window-[1-9]") . t)
	which-key-replacement-alist)

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

;;;* frames

;; creating, switching and deleting emacs frames
(use-package frame
  :ensure nil
  :general
  (my/leader-key
    :infix my/infix/frames
    "n" '(my/new-frame :which-key "new frame")
    "o" '(other-frame :which-key "switch frame")
    "d" '(delete-frame :which-key "delete frame"))
  :config
  (defun my/new-frame ()
    "Create a new frame and focus it."
    (interactive)
    (select-frame (make-frame))))

;;;* tabs

;; tabs are native feature of emacs since version 27
(when (>= emacs-major-version 27)
  ;; store and switch easily between different window configurations
  (use-package tab-bar
    :ensure nil
    :custom
    (tab-bar-show 1)                ; only display the tab-bar if there is more than one tab
    (tab-bar-close-button-show nil) ; don't show the close button
    (tab-bar-new-button-show nil)   ; don't show the "new tab" button
    (tab-bar-new-tab-to 'rightmost) ; always create new tabs on the rightmost position
    ;; use custom function for the tab naming which adds some padding around the buffer name
    (tab-bar-tab-name-function 'my//tab-bar-tab-name-current)
    :custom-face
    ;; adapt active tab to use the default background color
    (tab-bar-tab ((t (:background nil :box nil :inherit default))))
    :general
    (my/leader-key
      :infix my/infix/tabs
      "." '(hydra-tabs/body :which-key "[menu]")
      "n" '(tab-bar-new-tab :which-key "new")
      "d" '(my/close-tab :which-key my//close-tab-wk-replacement)
      "t" '(tab-bar-select-tab-by-name :which-key "switch")
      "r" '(my/tab-bar-rename-tab :which-key "rename"))
    :config
    ;; show the current tab name in the title bar of emacs
    (defun my//get-current-tab-name ()
      "Return the name of the current tab."
      (let* ((tabs (funcall tab-bar-tabs-function))
             (tab-index (tab-bar--current-tab-index tabs))
             (tab-name (alist-get 'name (nth tab-index tabs))))
        tab-name))
    (setq-default frame-title-format '(:eval (format "%s" (my//get-current-tab-name))))

    ;;
    ;; give the tabs some padding
    ;;

    (defun my//add-padding-to-tab-name (name)
      "Add some space characters around NAME."
      (concat "  " name "  "))

    (defun my//tab-bar-tab-name-current ()
      "Same as the default function `tab-bar-tab-name-current' but add some padding around the name."
      (my//add-padding-to-tab-name
       (buffer-name (window-buffer (minibuffer-selected-window)))))

    (defun my/tab-bar-rename-tab (name)
      "Call `tab-bar-rename-tab' but add some padding around the name beforehand."
      (interactive "sNew name for tab (leave blank for automatic naming): ")
      (tab-bar-rename-tab (if (s-blank? name) ; no padding for the "automatic naming" cases
                            name
                            (my//add-padding-to-tab-name name))))

    ;;
    ;; custom close tab function
    ;;

    (defun my//close-tab-wk-replacement (entry)
      "Which key replacement function for `my/close-tab' that checks for the current prefix argument."
      (let ((key (car entry)))
	(if prefix-arg
	  `(,key . "close (others)")
	  `(,key . "close (current)"))))

    (defun my/close-tab (arg)
      "Closes the current tab.
If the prefix ARG is set, close all other tabs instead."
      (interactive "P")
      (if arg
	(tab-bar-close-other-tabs)
	(tab-bar-close-tab)))
    ;;
    ;; menu hydra for more options
    ;;

    (defun my/tab-bar-move-tab-left ()
      "Move the current tab to the left."
      (interactive)
      (tab-bar-move-tab -1))

    (defun my/tab-bar-move-tab-right ()
      "Move the current tab to the right."
      (interactive)
      (tab-bar-move-tab 1))

    (defhydra hydra-tabs (:hint nil)
      "
^Navigation^   ^Movement^       ^Delete^      ^Misc^
^^^^^^^^---------------------------------------------------
_h_: go left   _H_: move left   _d_: current  _n_: new tab
_l_: go right  _L_: move right  _D_: others   _r_: rename
^^^^^^^^---------------------------------------------------
[_q_]: quit
^^^^^^^^---------------------------------------------------
"
      ("n" tab-bar-new-tab)
      ("d" tab-bar-close-tab)
      ("D" tab-bar-close-other-tabs)
      ("r" my/tab-bar-rename-tab)
      ("h" tab-bar-switch-to-prev-tab)
      ("l" tab-bar-switch-to-next-tab)
      ("H" my/tab-bar-move-tab-left)
      ("L" my/tab-bar-move-tab-right)
      ("q" nil))))

;;;* ivy/counsel/swiper

;; a generic completion frontend for emacs
(use-package ivy
  :custom
  (ivy-count-format "(%d/%d) ")          ; format for the number of candidates
  (ivy-use-virtual-buffers t)            ; enable virtual buffers (e.g. recent files & bookmarks)
  (ivy-magic-slash-non-match-action nil) ; allow "/" to create new non-existent directories
  (ivy-use-selectable-prompt t)          ; makes the prompt line (line 0) selectable
  (ivy-fixed-height-minibuffer t)        ; fixate the height of the minibuffer even if there are fewer candidates
  (ivy-read-action-format-function
   'ivy-read-action-format-columns)      ; use several columns for the actions docstring if needed
  :config
  ;;
  ;; minibuffer configuration for integration with `evil' states
  ;;

  ;; adapt keybindings for the `evil' states in the minibuffer
  (general-define-key
   :states '(normal insert emacs)
   :keymaps 'ivy-minibuffer-map
   ;; enable up and down navigation in ivy buffer with 'C-j' and 'C-k'
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line
   ;; rebind some generic `ivy' keybindings for all states
   "C-o" 'hydra-ivy/body
   "M-o" 'ivy-dispatching-done
   "M-O" nil
   "RET" 'ivy-done
   ;; add bindings to (un)mark candidates without using the `ivy-hydra'
   "M-m" 'ivy-mark
   "M-u" 'ivy-unmark)

  ;; close the ivy hydra when the minibuffer is closed to prevent weird states
  (add-hook 'minibuffer-exit-hook 'hydra-keyboard-quit)

  ;;
  ;; buffer switching configuration
  ;;

  (defun my/ivy-switch-buffer (&optional arg)
    "Call `ivy-switch-buffer' but ignore certain types of buffers if a prefix ARG is passed."
    (interactive "P")
    (let ((ivy-ignore-buffers (if arg
				  (append ivy-ignore-buffers
					  '("^ *\\*")    ; ignore all "system buffers"
					  '("^:")        ; ignore `dired-sidebar' buffers
					  '("^magit"))   ; ignore `magit' buffers
				ivy-ignore-buffers)))
      (ivy-switch-buffer)))

  (defun my//switch-buffer-wk-replacement (entry)
    "Which key replacment function for `my/ivy-switch-buffer' to indicate if a prefix argument is currently present."
    (let ((key (car entry)))
      (if prefix-arg
	`(,key . "switch (filtered)")
	`(,key . "switch (all)"))))

  (my/leader-key
    "bb" '(my/ivy-switch-buffer :which-key my//switch-buffer-wk-replacement))

  ;; shortcut keybinding to kill a buffer from ivy completion
  (general-define-key
   :states '(normal insert emacs)
   :keymaps 'ivy-switch-buffer-map
   "C-d" 'ivy-switch-buffer-kill)

  ;;
  ;; `ivy-occur' configuration
  ;;

  ;; keybindings to easily switch to `wgrep' from `ivy-occur'
  (my/major-mode-leader-key
    :keymaps 'ivy-occur-grep-mode-map
    "" '(:ignore t :which-key "Ivy Occur")
    "Y" '(my/copy-content-to-new-buffer :which-key "copy to new buffer")
    "W" '(ivy-wgrep-change-to-wgrep-mode :which-key "switch to wgrep"))

  (defun my/copy-content-to-new-buffer ()
    "Copy content of the current buffer into a new one and kill the old one."
    (interactive)
    (let ((old-buffer (current-buffer))
	  (new-buffer (generate-new-buffer "untitled")))
      (copy-to-buffer new-buffer (point-min) (point-max))
      (switch-to-buffer new-buffer)
      (fundamental-mode)
      (kill-buffer old-buffer)))

  (ivy-mode 1))

;; ivy interface for `xref' results
(use-package ivy-xref
  :after ivy
  :init
  ;; xref initialization is slightly different starting with emacs version 27
  ;; for more information see -> https://github.com/alexmurray/ivy-xref
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; collection of ivy enhanced versions of emacs commands
(use-package counsel
  :after (ivy transient)
  :general
  (my/leader-key
    "SPC" '(counsel-M-x :which-key "M-x"))

  (my/leader-key
    :infix my/infix/files
    "f" '(counsel-find-file :which-key "find file")
    "r" '(counsel-recentf :which-key "recent file"))

  (my/leader-key
    :infix my/infix/insert
    "c" '(my/insert-color-hex :which-key "color hex code"))

  (my/leader-key
    :infix my/infix/search
     "d" '(my/counsel-ag-transient :which-key "search in directory"))

  (my/leader-key
    :infix my/infix/jump
    "i" '(counsel-semantic-or-imenu :which-key "imenu"))
  :config
  (defun my/insert-color-hex ()
    "Insert a W3C color hex code."
    (interactive)
    (let* ((ivy-inhibit-action t) ; set `ivy-inhibit-action' to prevent any ivy action
	   (color (counsel-colors-web)))
      (counsel-colors-action-insert-hex color)))

  ;;
  ;; create a transient command for the `counsel-ag' function
  ;; enable the silver searcher command line arguments as configuration options
  ;;

  (defun my/ag-elsewhere-suffix (&optional args)
    "Choose a directory, then execute `counsel-ag' within it."
    (interactive (list (transient-args 'my/counsel-ag-transient)))
    (counsel-ag nil                                                ; no initial input
		(counsel-read-directory-name "Select directory: ") ; choose directory
		(s-join " " args)))                                ; pass ag arguments

  (transient-define-suffix my//ag-current-directory-suffix (args)
    "Execute a search with `counsel-ag' in the current default directory."
    :description (lambda () (concat (propertize default-directory 'face 'font-lock-keyword-face)
				    " (current default directory)"))
    (interactive (list (transient-args 'my/counsel-ag-transient)))
    (counsel-ag nil                 ; no initial input
		default-directory   ; use current `default-directory'
		(s-join " " args))) ; pass ag arguments

  (defun my//ag-select-file-extension (&rest _ignored)
    "Parse all available sg file extension options. Choose one of the available options via `ivy-read'."
    (let* (;; list all ag file types and extract parameter and description
	   ;; e.g. ("--actionscript" ".as  .mxml")
	   (ext-strings (-map #'cdr (s-match-strings-all "\\(--.*?\\)\n\s+\\(.*?\\)\n"
							 (shell-command-to-string "ag --list-file-types"))))
	   ;; calculate max length so we can use it for the alignment of the ivy candidates
	   (max-length (-max (-map #'seq-length (-map #'-first-item ext-strings))))
	   (formatted-type-strings (-map (lambda (x)
					   (let ((parameter (-first-item x))
						 (description (-second-item x)))
					     (concat
					      ;; remove leading dashes
					      (substring parameter 2)
					      ;; fill with spaces for a matching alignment
					      (s-repeat (- max-length (seq-length parameter)) " ")
					      " [" description "]")))
					 ext-strings))
	   (ext (condition-case nil
		    (ivy-read "Select file extension: " formatted-type-strings
			      :require-match t)
		  (quit nil)))) ; catch local quit within ivy completion
      (if (not ext)
	""
	(concat "--" (-first-item (s-split " " ext))))))

  (transient-define-argument my--ag-file-type ()
    :description "File Types"
    :class 'transient-option
    :key "T"
    :argument ""
    :reader 'my//ag-select-file-extension)

  (transient-define-argument my--ag-file-search-regex ()
    :description "Limit search to filenames matching PATTERN"
    :class 'transient-option
    :key "-G"
    :argument "--file-search-regex"
    :reader (lambda (&rest _ignored)
	      (concat " " (read-string "PATTERN: "))))

  (transient-define-argument my--ag-ignore-regex ()
    :description "Ignore files/directories matching PATTERN"
    :class 'transient-option
    :key "-I"
    :argument "--ignore"
    :reader (lambda (&rest _ignored)
	      (concat " " (read-string "PATTERN: "))))

  (transient-define-prefix my/counsel-ag-transient ()
    "Configure AG search parameters."
    ["Search Options"
     ("-f" "Follow symlinks" "--follow")
     (my--ag-file-search-regex)
     (my--ag-ignore-regex)
     ("-u" "Search ALL files" "--unrestricted")
     ("-U" "Ignore VCS ignore files" "--skip-vcs-ignores")
     ("-v" "Invert match" "--invert-match")
     ("-z" "Search contents of compressed (e.g. gzip) files" "--search-zip")
     (my--ag-file-type)]
    ["Execute search in"
     ("d" my//ag-current-directory-suffix)
     ("e" "elsewhere" my/ag-elsewhere-suffix)])

  (counsel-mode 1))

;; ivy enhanced alternative to `isearch'
(use-package swiper
  :after ivy
  :custom (swiper-goto-start-of-match t) ; put cursor on match start (instead of the end)
  :general
  (my/leader-key
    :infix my/infix/search
    "s" '(swiper :which-key "search in current file"))

  ;; pass currently selected region in `visual-state'
  (my/leader-key
    :states 'visual
    :infix my/infix/search
    "s" '(my/swiper-with-input :which-key "search in current file"))
  :config
  (defun my/swiper-with-input (start end)
    "Call `swiper' for search but with the inital input of the current region (START to END)."
    (interactive "r")
    (let ((region-string (buffer-substring start end)))
      (swiper region-string))))

;; style utilities to make ivy minibuffers more pretty
(use-package ivy-rich
  :after (ivy counsel all-the-icons)
  :defines all-the-icons-dir-icon-alist
  ;; set local `tab-width' for the minibuffer to ensure nice icon alignment
  :hook ((minibuffer-setup . (lambda () (setq-local tab-width 2))))
  :custom
  (ivy-rich-path-style 'abbrev)  ; show abbreviated paths
  (ivy-virtual-abbreviate 'full) ; abbreviation for virtual buffers
  (ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((my//ivy-rich-switch-buffer-icon (:width 2))
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success)) ; return project name using `projectile'
       ;; return file path relative to project root or `default-directory' if project is nil
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer (:width 45))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer (:width 40))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer (:width 40))
       (my//ivy-rich-variable-value (:width 20 :face font-lock-warning-face))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((my//ivy-rich-file-icon)
       (ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
      :delimiter "\t")
     counsel-find-file
     (:columns
      ((my//ivy-rich-file-icon)
       (ivy-rich-candidate (:width 0.8))))))

  :config
  ;;
  ;; expand `ivy-rich's capabilities with some custom columns:
  ;; - variable value strings
  ;; - buffer icons (based on major mode)
  ;; - file type icons
  ;;

  (defun my//ivy-rich-variable-value (candidate)
    "Display the value of the variable represented by CANDIDATE.
The result will be truncated to a max of 20 characters."
    (let* ((var (intern-soft candidate))                                   ; use `intern-soft' to avoid creation of a new variable
	   (var-value (if (boundp var) (symbol-value var) 'VOID_VARIABLE)) ; check if the variable is actually bound or void
	   (var-value-string (prin1-to-string var-value)))
      (concat " " var-value-string)))

  (defun my//ivy-rich-switch-buffer-icon (candidate)
    "Display the appropriate icon for the `major-mode' of CANDIDATE."
    (with-current-buffer (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	  (all-the-icons-icon-for-mode 'fundamental-mode) ; fall back to `fundamental-mode' icon
	  icon))))

  (defun my//ivy-rich-file-icon (candidate)
    "Display the appropriate icon for file type of CANDIDATE.
It covers some special cases for different directory types.
The code was \"inspired\" from this config: https://ladicle.com/post/config/"
    (let ((icon (if (file-directory-p candidate)
		    ;; special cases for directories
		    (cond
		     ;; - tramp
		     ((and (fboundp 'tramp-tramp-file-p)
			   (tramp-tramp-file-p default-directory))
		      (all-the-icons-octicon "file-directory"))
		     ;; - symlink folder
		     ((file-symlink-p candidate)
		      (all-the-icons-octicon "file-symlink-directory"))
		     ;; - git submodule
		     ((all-the-icons-dir-is-submodule candidate)
		      (all-the-icons-octicon "file-submodule"))
		     ;; - git repository
		     ((file-exists-p (format "%s/.git" candidate))
		      (all-the-icons-octicon "repo"))
		     ;; - default directory
		     (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
			  (apply (car matcher) (list (cadr matcher))))))
		  ;; in case of a "regular" file candidate
		  (all-the-icons-icon-for-file candidate))))
      (if (symbolp icon)
	(all-the-icons-faicon "file-o") ; fall back to basic file icon
	icon)))

  (ivy-rich-mode 1))

;;;* dired

;; [DIR]ectory [ED]itor for emacs
(use-package dired
  :ensure nil
  :hook (dired-mode . auto-revert-mode) ; automatically revert buffer on file changes
  :general
  (my/leader-key
    :infix my/infix/dired
    "d" '(dired :which-key "dired")
    "K" '(my/kill-all-dired-buffers :which-key "kill all dired buffers")
    "b" '(my/ivy-switch-to-dired-buffer :which-key "switch to dired buffer"))

  (my/major-mode-leader-key
    :keymaps '(dired-mode-map dired-sidebar-mode-map)
    "" '(:ignore t :which-key "Dired Mode")
    "." '(my//hydra/dired/body :which-key "[menu]")
    "w" '(dired-toggle-read-only :which-key "writable dired"))

  (general-define-key
    :keymaps 'dired-mode-map
    :states 'normal
    "_" 'my/dired-create-empty-file)

  :config
  ;; evil-integration.el makes `dired-mode-map' an override map
  ;; unbind SPC to make sure that our prefixes for `my/major-mode-leader-key' work correctly
  (general-unbind 'normal dired-mode-map "SPC")

  ;; `revert-buffer' and `dired-revert' do not always keep the current window position
  ;; to avoid uncontrollable changes of the current position we remap it to our own custom function
  (defun my/dired-revert (&rest _)
    "Wrapper around `dired-revert' but saves the current window position.
This function is inspired from `dired-sidebar-revert' but adopted for the usage with a current dired buffer."
    (interactive)
    (when (not (derived-mode-p 'dired-mode))
      (user-error "Not a 'dired' buffer"))
    (let ((old-window-start (window-start)))
      (dired-revert)
      (set-window-start (selected-window) old-window-start)))

  (general-define-key
   :keymaps 'dired-mode-map
   [remap revert-buffer] 'my/dired-revert)

  ;; remap the default `dired-do-copy', `dired-do-delete', `dired-do-rename' and `dired-create-directory' functions to our own implementations
  ;; these revert the buffer afterwards to ensure that the dired buffer content is always up to date and the icons are correctly displayed
  (defun my/dired-do-copy ()
    "Replacement function for `dired-do-copy' which does revert the buffer afterwards."
    (interactive)
    (dired-do-copy)
    (my/dired-revert))

  (defun my/dired-do-delete ()
    "Replacement function for `dired-do-delete' which does revert the buffer afterwards."
    (interactive)
    (dired-do-delete)
    (my/dired-revert))

  (defun my/dired-create-directory ()
    "Replacement function for `dired-create-directory' which does revert the buffer afterwards."
    (interactive)
    (call-interactively 'dired-create-directory)
    (my/dired-revert))

  (defun my/dired-do-rename ()
    "Replacement function for `dired-do-rename' which does revert the buffer afterwards."
    (interactive)
    (call-interactively 'dired-do-rename)
    (my/dired-revert))

  (my/normal-state-keys
    :keymaps 'dired-mode-map
    [remap dired-do-copy] 'my/dired-do-copy
    [remap dired-do-delete] 'my/dired-do-delete
    [remap dired-do-rename] 'my/dired-do-rename
    [remap dired-create-directory] 'my/dired-create-directory)

  ;;
  ;; some utility functions for dired
  ;;

  (defun my/kill-all-dired-buffers ()
    "Kill all currently opened `dired' buffers."
    (interactive)
    (let ((dired-buffers (-filter (-compose (-partial 'eq 'dired-mode)
					    (-partial 'buffer-local-value 'major-mode)) (buffer-list))))
      (when (yes-or-no-p (concat "Do you really want to kill all "
				 (number-to-string (length dired-buffers))
				 " 'dired' buffers?"))
	(mapc 'kill-buffer dired-buffers))))

  (defun my/dired-create-empty-file ()
    "Create an empty file within the current `dired' buffer.
This function respects the current (subtree) directory."
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
    "Call `ivy-switch-buffer' but show only currently opened `dired' buffers."
    (interactive)
    (let ((ivy-use-virtual-buffers nil)    ; don't show virtual buffers
	  (ivy-use-ignore-default 'always) ; don't fall back to regular buffer list if no dired buffers exist
	  (ivy-ignore-buffers (append ivy-ignore-buffers '(my//only-dired-buffers))))
      (ivy-switch-buffer)))

  ;; hydra to remember some of the more useful bindings available for `dired' based on `evil-collection'
  ;; since this is just a snippet of all available functions it could be adapted by removing unused and/or adding other commands
  (defhydra my//hydra/dired (:hint nil)
    "
^Navigation^    ^Create^            ^Mark^                  ^Commands^        ^Open^                      ^Misc^
^-^-------------^-^-----------------^-^---------------------^-^---------------^-^-------------------------^-^---------
_j_: next line  ___: new file       _m_:  mark              _C_: copy         _RET_:   open               _i_: wdired
_k_: prev line  _+_: new directory  _u_:  unmark            _D_: delete       _S-RET_: open other window  _=_: diff
^ ^             ^ ^                 _U_:  unmark            _M_: chmod        _W_:     open default
^ ^             ^ ^                 _*/_: mark directories  _R_: rename
^ ^             ^ ^                 _*%_: mark files regex  _X_: shell cmd
^ ^             ^ ^                 _*t_: toggle marks      _Z_: compress
^ ^             ^ ^                 ^ ^                     _c_: compress to
^-^-------------^-^-----------------^-^---------------------^-^---------------^-^-------------------------^-^---------
[_q_]: quit
^-^-------------^-^-----------------^-^---------------------^-^---------------^-^-------------------------^-^---------
"
    ;; navigation
    ("j" dired-next-line)
    ("k" dired-previous-line)
    ;; create
    ("_" my/dired-create-empty-file)
    ("+" dired-create-directory)
    ;; mark
    ("m" dired-mark)
    ("u" dired-unmark)
    ("U" dired-unmark-all-marks)
    ("*/" dired-mark-directories)
    ("*%" dired-mark-files-regexp)
    ("*t" dired-toggle-marks)
    ;; commands
    ("C" dired-do-copy)
    ("D" dired-do-delete)
    ("M" dired-do-chmod)
    ("R" dired-do-rename)
    ("X" dired-do-shell-command)
    ("Z" dired-do-compress)
    ("c" dired-do-compress-to)
    ;; open
    ("RET" dired-find-file)
    ("S-RET" dired-find-file-other-window)
    ("W" browse-url-of-dired-file)
    ;; misc
    ("i" dired-toggle-read-only)
    ("=" dired-diff)
    ("q" nil)))

;; make the dired buffer editable for renaming files and folders
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

;; show icons for files and directories
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; multi stage copy/pasting of files
(use-package dired-ranger
  :general
  (my/major-mode-leader-key
    :keymaps '(dired-mode-map dired-sidebar-mode-map)
    :infix "r"
    "" '(:ignore t :which-key "Ranger")
    "y" '(dired-ranger-copy :which-key "copy")
    "Y" '(my/dired-ranger-copy-add :which-key "add to copy")
    "m" '(dired-ranger-move :which-key "move")
    "p" '(my/dired-ranger-paste :which-key "paste"))

  (general-define-key
   :keymaps '(dired-mode-map dired-sidebar-mode-map)
   :states 'normal
   :prefix "C-r"
   "" '(:ignore t :which-key "Dired-Ranger")
   "y" '(dired-ranger-copy :which-key "copy")
   "Y" '(my/dired-ranger-copy-add :which-key "add to copy")
   "m" '(dired-ranger-move :which-key "move")
   "p" '(my/dired-ranger-paste :which-key "paste"))

  :config
  (defun my/dired-ranger-copy-add ()
    "Call `dired-ranger-copy' with raw prefix arg to add the selected files to the last copy ring entry."
    (interactive)
    (dired-ranger-copy t))

  (defun my/dired-ranger-paste ()
    "Call `dired-ranger-paste' with raw prefix arg to prevent the clipboard to be cleared."
    (interactive)
    (dired-ranger-paste t)))

;; filtering of files in dired buffers
(use-package dired-narrow
  :general
  (general-define-key
    :keymaps 'dired-mode-map
    :states 'normal
    "/" 'dired-narrow))

;; render directory subtree inside dired buffer
(use-package dired-subtree
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "->")
  :general
  (general-define-key
    :keymaps 'dired-mode-map
    :states 'normal
    "TAB" 'my/dired-subtree-toggle)
  :config
  (defun my/dired-subtree-toggle ()
    "Refresh buffer after `dired-subtree-toggle' to ensure the icons are loaded correctly."
    (interactive)
    (dired-subtree-toggle)
    (my/dired-revert))

  ;; `evil-collection' will remap subtree bindings after the package was loaded
  ;; make sure that the subtree custom functions are being used instead
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   [remap dired-subtree-toggle] 'my/dired-subtree-toggle))

;; neotree like sidebar using dired
(use-package dired-sidebar
  :hook
  ;; make all sidebar windows resizeable and do not wrap long lines
  (dired-sidebar-mode . (lambda ()
			  ;; both variables are buffer-local by default
			  (setq truncate-lines t)        ; do not wrap lines into the next visible one
			  (setq window-size-fixed nil))) ; make window resizeable
  :custom
  (dired-sidebar-refresh-on-projectile-switch nil) ; do not refresh the sidebar on project switch
  (dired-sidebar-toggle-hidden-commands nil)       ; don't hide sidebar during certain commands (caused problems with `balance-windows')

  :general
  (my/leader-key
    "r" '(dired-sidebar-toggle-sidebar :which-key my//sidebar-which-key-replacement))

  :init
  (defun my//sidebar-which-key-replacement (entry)
    "Which key replacement function for the `dired-sidebar'."
    (let ((key (car entry)))
      (if (and
	   (fboundp 'dired-sidebar-showing-sidebar-p)
	   (dired-sidebar-showing-sidebar-p))
	`(,key . "sidebar close")
	`(,key . "sidebar open"))))

  :config
  ;; set custom `which-key' description for `dired-sidebar-mode'
  ;; the actual keybindings are inherited from `dired-mode-map'
  (my/major-mode-leader-key
    :keymaps 'dired-sidebar-mode-map
    "" '(:ignore t :which-key "Dired Sidebar"))

  ;; explicitly set `wdired' description for `which-key'
  ;; needed to correctly work with the "dired-sidebar + wdired" hack
  (my/major-mode-leader-key
    :keymaps 'wdired-mode-map
    :major-modes 'dired-sidebar-mode
    "" '(:ignore t :which-key "Wdired")
    "c" '(wdired-finish-edit :which-key "finish edit")
    "k" '(wdired-abort-changes :which-key "cancel"))

  ;; do not resize the sidebar window after toggling
  (add-to-list 'window-size-change-functions
                  (lambda (_)
                    (let ((sidebar-window (dired-sidebar-showing-sidebar-p)))
                      (unless (null sidebar-window)
                        (setq dired-sidebar-width (window-width sidebar-window))))))

  ;; make sure that derived dired modes return the correct directory
  ;; (e.g. `dired-do-rename' in sidebar buffer will now prefill the correct path)
  (defun my//dired-dwim-target-directory-advice (orig-fn)
    (ignore orig-fn) ; avoid compiler warnings
    (if (derived-mode-p 'dired-mode)
      (dired-current-directory)
      (orig-fn)))
  (advice-add 'dired-dwim-target-directory :around 'my//dired-dwim-target-directory-advice)

  ;; remap sidebar specific functions
  (my/normal-state-keys
    :keymaps 'dired-sidebar-mode-map
    [remap dired-up-directory] 'dired-sidebar-up-directory
    [remap quit-window] 'dired-sidebar-toggle-sidebar)

  ;; map sidebar window to window number zero
  (with-eval-after-load 'winum
    (defun winum-assign-0-to-sidebar ()
      "Always assign any 'dired-sidebar' window to winum number zero."
      (when (eq major-mode 'dired-sidebar-mode) 0))
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-sidebar)))

;;;* git

;; use git with ease from within emacs
(use-package magit
  :general
  (my/leader-key
    :infix my/infix/git
    "s" '(magit-status :which-key "status")
    "b" '(magit-blame :which-key "blame"))

  ;; keybindings for the commit message editor
  (my/major-mode-leader-key
    :keymaps 'with-editor-mode-map
    :major-modes 'text-mode
    "" '(:ignore t :which-key "Editor")
    "c" '(with-editor-finish :which-key "editor finish")
    "k" '(with-editor-cancel :which-key "editor cancel"))

  :custom
  ;; this makes magit ask us whether we want to create a PR after we pushed a new branch to stash/bitbucket
  ;; if the pull request creation is confirmed it will open the corresponding webpage in the browser
  (magit-process-prompt-functions #'my//magit-process-ask-create-bitbucket-pull-request)

  :config
  ;; NOTE this is exclusively working with stash/bitbucket, for other hosts the regex would probably need some adaptions
  (defconst my--magit-process-create-bitbucket-pull-request-regexp
    "remote: Create pull request for.*\nremote: +\\(?1:[^ ]+\\)[^\n]*")

  (defun my//magit-process-ask-create-bitbucket-pull-request (_ string)
    "Check if the STRING match the pull request regex and browse to this URL if desired."
    (when (string-match my--magit-process-create-bitbucket-pull-request-regexp string)
      (let ((url (match-string 1 string))
            (inhibit-message t))
	(if (y-or-n-p "Create PR? ")
            (browse-url (url-encode-url url)))))))

;; browse corresponding page on github/gitlab/bitbucket/etc. from an emacs buffer
;; this works with several buffer types like:
;; - file buffer
;; - dired
;; - magit (representing code)

;; the package can work with the most popular remote types (e.g. github, gitlab, etc.) out of the box (see `browse-at-remote-remote-type-domains')
;; if you have a specific git domain not in that list (e.g. github enterprise) the mapping will not work
;; to solve this issue you can set the repository type directly in your git config:
;;
;; git config --add browseAtRemote.type "github" (for the current repository only)
;; git config --global --add browseAtRemote.type "stash" (for all your repositories)
(use-package browse-at-remote
  :general
  (my/leader-key
    :infix my/infix/git
    "o" '(browse-at-remote :which-key "browse remote")))

;;;* projects

;; project management within emacs is usually done via the `projectile' package
;; the concept of a project is simply a folder containing some special file(s)

;; some project types are supported out of the box:
;; - git
;; - maven
;; - mercurial
;; - etc.
;; you can also add an emtpy '.projectile' to a folder in order to mark it as a project

;; `projectile' will then provide a nice set of features to operate on a project:
;; - jump to a file in project
;; - jump to a project buffer
;; - switch between projects
;; - etc.

(use-package projectile
  :defer 1
  :config
  ;; special configuration for Dart projects
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
  (my/leader-key
    :infix my/infix/projects
    "K" '(projectile-kill-buffers :which-key "kill project"))

  (projectile-mode 1))

;; `counsel-projectile' provides further `ivy' integration for `projectile'
;; it also defines replacements for existing projectile commands as well as new commands
(use-package counsel-projectile
  :general
  (my/leader-key
    :infix my/infix/projects
    "p" '(counsel-projectile-switch-project :which-key "switch project")
    "b" '(counsel-projectile-switch-to-buffer :which-key "switch project buffer")
    "f" '(counsel-projectile-find-file :which-key "find project file")
    "s" '(counsel-projectile-ag :which-key "search in project"))

  :config
  ;; open project root dired buffer from within counsel completion
  (defun my//counsel-projectile-projectile-dired (_)
    "Wrapper around `projectile-dired' so it can be used as an alternate action for `counsel-projectile'."
    (projectile-dired))
  (ivy-add-actions 'counsel-projectile
		   '(("d" my//counsel-projectile-projectile-dired "dired project root")))
  (ivy-add-actions 'counsel-projectile-find-file
		   '(("d" my//counsel-projectile-projectile-dired "dired project root")))

  (counsel-projectile-mode))

;;;* editing

;; everything related to (text) editing

;;;** brackets

;; highlighting matching parentheses
(use-package paren
  :ensure nil
  :custom
  (show-paren-style 'expression)
  :custom-face
  (show-paren-match-expression ((t (:weight ultra-bold :inherit warning))))
  :config
  (show-paren-mode))

;; insert closing delimiter automatically
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-local-mode)))

;; mark nested parentheses with different colors
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode))
  :init
  (defun my//make-rainbow-delimiters-more-colorful ()
    "Make the rainbow colors more saturated.

This hack is based on the code from
https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/"
    (dolist (index (number-sequence 1 rainbow-delimiters-max-face-count) nil)
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
        (cl-callf color-saturate-name (face-foreground face) 50)))))

;;;** snippets

;; YASnippet (Yet Another Snippet) is a template system for emacs
;; it allows to type a abbreviation and automatically expand it into function templates

;; basic package for snippet insertion
(use-package yasnippet
  :defer 1
  :defines company-backend
  :custom-face
  ;; make yasnippet fields more standing out
  (yas-field-highlight-face ((t (:background nil :inherit highlight))))
  :config
  (my/leader-key
    :infix my/infix/insert
    "s" '(yas-insert-snippet :which-key "snippet"))

  (general-define-key
   :states 'insert
   "C-o" 'yas-expand
   "C-S-o" 'company-yasnippet)

  (general-define-key
   :keymaps 'company-active-map
   "C-S-o" 'my/toggle-between-company-and-yasnippet)

  ;; TAB should only be used for indentation or completion
  (general-unbind
    :keymaps 'yas-minor-mode-map
    "TAB"
    "<tab>")

  (defun my/toggle-between-company-and-yasnippet ()
    "Switch between the currently used company backend and `company-yasnippet' (or vice versa).
This works by aborting the currently active completion via `company-abort' and calling either `company-complete' or `company-yasnippet'."
    (interactive)
    (if (eq company-backend 'company-yasnippet)
      (progn
	(company-abort)
	(company-complete))
      (progn
	(company-abort)
	(condition-case nil (call-interactively 'company-yasnippet)
	  ;; catch the error if no snippets are available
	  (user-error (progn
			(company-complete)
			(message "No snippets found!")))))))

  (yas-global-mode 1))

;; collection of general useful snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; collection of java specific snippets
(use-package java-snippets
  :after yasnippet)

;;;** spellcheck

;; emacs handles spell-checking and corrections of words, regions or buffers via the built-in `ispell' package.
;; the actual checking is handled by one of three supported external checker programs:
;; - Hunspell
;; - GNU Aspell
;; - Ispell
;; (emacs will by default choose aspell over hunspell over ispell)

(use-package ispell
  :ensure nil
  :config
  (defun my//get-system-LC_MESSAGES-value ()
    "Extract the system default language from LC_MESSAGES."
    (-second-item (s-match "LC_MESSAGES=\"\\(.*?\\)\\..*?\""
			   (shell-command-to-string "locale"))))

  (defun my//ispell-local-dict-wk-replacement (entry)
    "Which key replacement function to show the current local `ispell' dictionary."
    (let ((key (car entry))
	  (dict (if ispell-local-dictionary
		  ispell-local-dictionary
		  (my//get-system-LC_MESSAGES-value))))
      `(,key . ,(format "local [%s]" dict))))

  (defun my//ispell-global-dict-wk-replacement (entry)
    "Which key replacement function to show the current global `ispell' dictionary."
    (let ((key (car entry))
	  (dict (if ispell-dictionary
		  ispell-dictionary
		  (my//get-system-LC_MESSAGES-value))))
      `(,key . ,(format "global [%s]" dict))))

  (defun my/ispell-change-global-directory ()
    "Call `ispell-change-dictionary' interactively with prefix arg to change the global `ispell' dictionary."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'ispell-change-dictionary)))

  (my/leader-key
    :infix my/infix/spellcheck
    "" '(:ignore t :which-key "Spellcheck")
    "d" '(:ignore t :which-key "Dictionaries")
    "dl" '(ispell-change-dictionary :which-key my//ispell-local-dict-wk-replacement)
    "dg" '(my/ispell-change-global-directory :which-key my//ispell-global-dict-wk-replacement)))

;; `flyspell' enables on-the-fly spell-checking within emacs
;; incorrect words will be highlighted as soon as they are completed or as soon as the cursor hits a new word
(use-package flyspell
  :ensure nil
  :init
  (defun my//flyspell-mode-wk-replacement (entry)
    "Which key replacement for the `flyspell-mode' function."
    (let ((key (car entry)))
      (if (bound-and-true-p flyspell-mode)
	`(,key . "[X] flyspell mode")
	`(,key . "[ ] flyspell mode"))))

  (defun my//flyspell-prog-mode-wk-replacement (entry)
    "Which key replacement for the `flyspell-prog-mode' function."
    (let ((key (car entry)))
      (if (bound-and-true-p flyspell-prog-mode)
	`(,key . "[X] flyspell prog mode")
	`(,key . "[ ] flyspell prog mode"))))
  :config
  ;; `flyspell' offers the function `flyspell-goto-next-error' which moves the cursor forward to the next error
  ;; unfortunately there is no function to move the cursor back to the previous error in the same behavior
  ;; the solution to this is the following function which is a slightly modified version of the one found on:
  ;; - http://pragmaticemacs.com/emacs/jump-back-to-previous-typo
  (defun my/flyspell-goto-previous-error (arg)
    "Go to ARG previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
	(when (and (eq (current-buffer) flyspell-old-buffer-error)
		   (eq pos flyspell-old-pos-error))
	  (when (= flyspell-old-pos-error min)
            ;; go to beginning of buffer
            (message "Restarting from end of buffer")
            (goto-char (point-max)))
	  (backward-word 1)
          (setq pos (point)))
	;; seek the next error
	(while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
			(if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
	;; save the current location for next invocation
	(setq arg (1- arg))
	(setq flyspell-old-pos-error pos)
	(setq flyspell-old-buffer-error (current-buffer))
	(goto-char pos)
	(if (= pos min)
            (progn
              (message "No more miss-spelled word!")
              (setq arg 0))))))

  (defhydra hydra-spellcheck (:hint nil)
    "
^Movement^          ^Actions^             ^Other^
^^^^--------------------------------------------------
_n_: next error     _F_: flyspell buffer  _z_: center
_N_: previous error _c_: correct word
^^^^--------------------------------------------------
[_q_]: quit
^^^^--------------------------------------------------
"
    ("n" flyspell-goto-next-error)
    ("N" my/flyspell-goto-previous-error)
    ("c" flyspell-correct-at-point)
    ("F" flyspell-buffer)
    ("z" evil-scroll-line-to-center)
    ("q" nil))

  (my/leader-key
    :infix my/infix/spellcheck
    "e" '(hydra-spellcheck/body :which-key "[errors]")
    "b" '(flyspell-buffer :which-key "buffer")
    "r" '(flyspell-region :which-key "region")
    "f" '(:ignore t :which-key "Flyspell Modes")
    "ff" '(flyspell-mode :which-key my//flyspell-mode-wk-replacement)
    "fp" '(flyspell-prog-mode :which-key my//flyspell-prog-mode-wk-replacement)))

;; correct misspelled words with `flyspell' using favorite interface (here: `ivy')
(use-package flyspell-correct
  :after flyspell
  :general
  (my/leader-key
    :infix my/infix/spellcheck
    "w" '(flyspell-correct-at-point :which-key "word"))
  :config
  ;; guarantee that `ispell' is correctly initialized
  (advice-add 'flyspell-correct-at-point :before 'ispell-set-spellchecker-params))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;;;* org

(defvar my--org-notes-folder  "~/Documents/Org/"
  "Directory containing ALL org note files.")
(defvar my--org-journal-folder  "~/Documents/Org/Journal/"
  "Directory for the `org-journal' files.")
(defvar my--org-todo-file "~/Documents/Org/GTD/todo.org"
  "GTD todo file which contains all tasks and projects.")
(defvar my--org-archive-file "~/Documents/Org/GTD/archive.org"
  "Archive file for tasks and projects which were done or canceled.")
(defvar my--org-calendar-file "~/Documents/Org/GTD/calendar.org"
  "File for repeating appointments (e.g. birthdays) and habits.")
(defvar my--org-ideas-file "~/Documents/Org/GTD/ideas.org"
  "Storage file for ideas and 'someday/maybe' projects.")

(use-package org
  :ensure nil
  :hook
  (org-mode . org-hide-block-all)                       ; leave all blocks collapsed by default
  (org-capture-mode . (lambda nil (evil-insert-state))) ; initial insert-state for `org-capture'
  :init
  (defun my/goto-org-todo-file ()
    "Jump to my GTD todo file."
    (interactive)
    (find-file my--org-todo-file))

  (defun my/goto-org-ideas-file ()
    "Jump to my ideas file."
    (interactive)
    (find-file my--org-ideas-file))

  (defun my/goto-org-calendar-file ()
    "Jump to my calendar file."
    (interactive)
    (find-file my--org-calendar-file))

  (defun my/goto-org-archive-file ()
    "Jump to my archive file."
    (interactive)
    (find-file my--org-archive-file))

  (defun my//org-emphasis-markers-wk-replacement (entry)
    "Which key replacement function for `my/org-toggle-emphasis'."
    (let ((key (car entry)))
      (if org-hide-emphasis-markers
        `(,key . "[ ] emphasis markers")
        `(,key . "[X] emphasis markers"))))

  (defun my//org-inline-images-wk-replacement (entry)
    "Which key replacement function for `my/org-toggle-inline-images'."
    (let ((key (car entry)))
      (if org-inline-image-overlays
        `(,key . "[X] inline images")
        `(,key . "[ ] inline images"))))

  (defun my//org-insert-link-wk-replacement (entry)
    (let ((key (car entry)))
      (if prefix-arg
        `(,key . "link (file)")
        `(,key . "link (any)"))))
  :custom
  (org-blank-before-new-entry '((heading . t)       ; ALWAYS set a blank line before a new heading
                                (plain-list-item))) ; NEVER set a blank line before a new list item
  ;;
  ;; todo keyword configuration
  ;;
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-enforce-todo-dependencies t)                 ; to mark a parent task as done all children must be done first
  (org-log-into-drawer t)                           ; insert state change notes and timestamps into a drawer (defaul: LOGBOOK)
  (org-log-done 'note)                              ; log the time when a task is marked as done
  (org-log-redeadline 'note)                        ; log the time when a deadline was changed
  (org-log-reschedule 'note)                        ; log the time when a schedule was changed
  ;;
  ;; agenda configuration
  ;;
  (org-agenda-files (list my--org-todo-file
                          my--org-calendar-file))
  (org-agenda-todo-list-sublevels nil) ; only show top level todos
  (org-agenda-custom-commands '(("c" "Custom agenda"
                                 ((tags "PRIORITY=\"A\""
                                        ((org-agenda-skip-function
                                          '(org-agenda-skip-entry-if 'todo 'done))              ; do not show "done" tasks
                                         (org-agenda-overriding-header "High-priority unfinished tasks:")))
                                  (agenda "" ((org-agenda-span 'day)))                          ; only show the current day
                                  (alltodo ""
                                           ((org-agenda-skip-function
                                             '(or
                                               (org-agenda-skip-subtree-if 'regexp "\\[#A\\]")  ; all tasks with priority "A"
                                               (org-agenda-skip-if nil '(scheduled deadline)))) ; all tasks with a schedule or deadline
                                            (org-agenda-overriding-header "ALL normal priority tasks:")))))))
  ;;
  ;; refile configuration
  ;;
  (org-refile-targets '((nil :maxlevel . 9)                 ; same file up to level 9 (should cover "all" headings)
                        (org-agenda-files     :level . 1)   ; all org-agenda files
                        (my--org-archive-file :level . 1))) ; the archive file
  (org-refile-use-outline-path 'file)        ; show file name and all top levels for refile targets
  (org-outline-path-complete-in-steps nil)   ; complete refile in one step
  (org-refile-allow-creating-parent-nodes t) ; allow creation of new headings during refile
  ;;
  ;; org-capture configuration
  ;;
  (org-capture-templates
   `(("t" "Task" entry (file my--org-todo-file)
      ,(concat "* %^{Type|TODO|NEXT} %?\n"
               "  :PROPERTIES:\n"
               "  :CATEGORY: %^{Category}\n"
               "  :CREATED: %u\n"
               "  :END:")
      :empty-lines 1)
     ("i" "Idea" entry (file my--org-ideas-file)
      ,(concat "* %? :idea:\n"
               "  :PROPERTIES:\n"
               "  :CATEGORY: %^{Category}\n"
               "  :END:"))
     ("s" "Scheduled" entry (file my--org-calendar-file)
      ,(concat "* %?\n"
               "  SCHEDULED: %^t\n"
               "  :PROPERTIES:\n"
               "  :CATEGORY: %^{Category}\n"
               "  :END:"))))

  :general
  (my/leader-key
    :infix my/infix/org
    "a" '(org-agenda :which-key "agenda")
    "c" '(org-capture :which-key "capture")
    "T" '(my/goto-org-todo-file :which-key "todos")
    "I" '(my/goto-org-ideas-file :which-key "ideas")
    "A" '(my/goto-org-archive-file :which-key "archive")
    "C" '(my/goto-org-calendar-file :which-key "calendar"))

  (my/major-mode-leader-key
    :keymaps '(org-mode-map org-journal-mode-map)
    "" '(:ignore t :which-key "Org")
    "TAB" '(my/org-show-current-heading-tidily :which-key "narrow")
    ;; these keybindings are based on the default bindings (C-c C-<key>) of org-mode
    "m" '(org-ctrl-c-ctrl-c :which-key "ctrl-c ctrl-c")
    "o" '(org-open-at-point :which-key "open at point")
    "w" '(org-refile :which-key "refile")
    "s" '(org-schedule :which-key "schedule")
    "d" '(org-deadline :which-key "deadline")
    "q" '(org-set-tags-command :which-key "set tags")
    "," '(org-priority :which-key "priority")
    "t" '(org-todo :which-key "todo")
    ;; Toggles
    "T" '(:ignore t :which-key "Toggles")
    "Ti" '(my/org-toggle-inline-images :which-key my//org-inline-images-wk-replacement)
    "Te" '(my/org-toggle-emphasis :which-key my//org-emphasis-markers-wk-replacement)
    ;; Insert
    "i" '(:ignore t :which-key "Insert & Edit")
    "i!" '(org-time-stamp-inactive :which-key "inactive timestamp")
    "i," '(org-insert-structure-template :which-key "structure template")
    ;; Insert -> Tables
    "it" '(:ignore t :which-key "Tables")
    "ita" '(org-table-align :which-key "align")
    "itt" '(org-table-create-or-convert-from-region :which-key "table")
    "ite" '(hydra-org-table/body :which-key "[edit]")
    ;; Insert -> Links
    "il" '(:ignore t :which-key "Links")
    "ile" '(hydra-org-links/body :which-key "[edit]")
    "ill" '(org-insert-link :which-key my//org-insert-link-wk-replacement)
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
  ;; automatically log the creation property for tasks
  (defun my//log-todo-creation-date (&rest _)
    "Log TODO creation date as 'CREATED' property.
For more information see: https://emacs.stackexchange.com/a/35776"
    (when (and (org-get-todo-state)
               (not (org-entry-get nil "CREATED")))
      (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a]"))))

  (add-hook 'org-after-todo-state-change-hook #'my//log-todo-creation-date)

  ;;
  ;; utility functions
  ;;

  (defun my/org-toggle-emphasis ()
    "Toggle the visibility of the org emphasis markers."
    (interactive)
    (setq-local org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (font-lock-flush))

  (defun my/org-toggle-inline-images ()
    "Call `org-toggle-inline-images' with prefix arg."
    (interactive)
    (org-toggle-inline-images t))

  ;; unfold current entry and close others
  ;; the source is from stackoverflow:
  ;; - https://stackoverflow.com/q/25161792
  (defun my/org-show-current-heading-tidily ()
    "Unfold current entry and keep other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
        ;; if the entry is hidden, unfold it completely
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

  ;;
  ;; individual functions for `org-emphasize'
  ;;

  (defun my/org-bold ()
    "Emphasize the selected region as bold."
    (interactive)
    (org-emphasize ?*))

  (defun my/org-italic ()
    "Emphasize the selected region as italic."
    (interactive)
    (org-emphasize ?/))

  (defun my/org-code ()
    "Emphasize the selected region as code."
    (interactive)
    (org-emphasize ?~))

  (defun my/org-strike-through ()
    "Emphasize the selected region as strike through."
    (interactive)
    (org-emphasize ?+))

  (defun my/org-underline ()
    "Emphasize the selected region as underlined."
    (interactive)
    (org-emphasize ?_))

  (defun my/org-verbatim ()
    "Emphasize the selected region as verbatim."
    (interactive)
    (org-emphasize ?=))

  (defun my/org-clear ()
    "Clear the selected region."
    (interactive)
    (org-emphasize ? ))

  ;;
  ;; hydra for editing org-links
  ;;

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

  ;;
  ;; utility functions for tables
  ;;

  (defhydra hydra-org-table (:hint nil)
    "
^Column^            ^Row^               ^HLine^              ^Misc^
^^^^^^^^---------------------------------------------------------------
_ch_: insert left   _rj_: insert below  _hj_: insert below   _S_: sort
_cl_: insert right  _rk_: insert above  _hk_: insert above
_cd_: delete
^^^^^^^^---------------------------------------------------------------
[_q_]: quit
^^^^^^^^---------------------------------------------------------------
"
    ("ch" my/org-insert-column-left)
    ("cl" org-table-insert-column)
    ("cd" org-table-delete-column)
    ("rj" my/org-insert-row-below)
    ("rk" org-table-insert-row)
    ("hj" org-table-insert-hline)
    ("hk" my/org-insert-hline-above)
    ("S" org-table-sort-lines)
    ("q" nil))

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
    (org-table-move-row-down)))

;; quick and easy completion of structure templates
;; prior to Org v9.2 this was done by `org-try-structure-completion'
;; the full list of possible shortcuts consists of `org-structure-template-alist' and `org-tempo-keywords-alist'
;;
;; for example enter "<e" and hit TAB to expand the line to an example block
(use-package org-tempo
  :after org
  :ensure nil)

(use-package evil-org
  :after (org evil)
  :defer t
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode))
  :config
  ;; this has to be called before all other keybindings, otherwise it could overwrite them
  (evil-org-set-key-theme '(navigation
                            additional
                            textobjects
                            calendar
                            return
                            shift))

  ;; make (evil)org-mode work with `drag-stuff'
  (when (package-installed-p 'drag-stuff)
    (defun my/org-drag-stuff-down (&optional arg)
      "Either drag stuff down or edit a present org-clock-timestap."
      (interactive "p")
      (if (and (org-at-clock-log-p)
               (org-at-timestamp-p 'lax))
          (org-shiftcontroldown arg)
        (drag-stuff-down arg)))

    (defun my/org-drag-stuff-up (&optional arg)
      "Either drag stuff up or edit a present org-clock-timestap."
      (interactive "p")
      (if (and (org-at-clock-log-p)
               (org-at-timestamp-p 'lax))
          (org-shiftcontrolup arg)
        (drag-stuff-up arg)))

    (general-define-key
     :definer 'minor-mode
     :keymaps 'evil-org-mode
     :states '(normal visual)
     "C-S-j" 'my/org-drag-stuff-down
     "C-S-k" 'my/org-drag-stuff-up)

    (general-define-key
     :definer 'minor-mode
     :keymaps 'evil-org-mode
     :states 'visual
     "C-S-h" 'drag-stuff-left
     "C-S-l" 'drag-stuff-right))

  ;;
  ;; some more utility bindings
  ;;

  (general-define-key
   :keymaps 'org-mode-map
   :states 'insert
   ;; use `evil-org' specific versions for open below/above
   "M-o" 'evil-org-open-below
   "M-O" 'evil-org-open-above)

  (defun my/evil-org-insert-subheading ()
    "Insert a new blank subheading.
Ensure to not split the current line."
    (interactive)
    (evil-org-append-line 1)
    (org-insert-subheading nil))

  (my/major-mode-leader-key
    :keymaps 'org-mode-map
    "ih" '(evil-org-org-insert-heading-respect-content-below :which-key "heading")
    "is" '(my/evil-org-insert-subheading :which-key "subheading")))

;; set up some sane default `evil' bindings for `org-agenda'
;; this package is part of `evil-org' but needs to be loaded separately
(use-package evil-org-agenda
  :after evil-org
  :ensure nil
  :config
  (evil-org-agenda-set-keys)

  ;; add missing bindings to page through weeks
  (general-define-key
    :keymaps 'org-agenda-mode-map
    :states 'motion
    "l" 'org-agenda-later
    "h" 'org-agenda-earlier))

;; simple org-mode based journaling mode
(use-package org-journal
  ;; carry over all items that do not emphasize an entry as done
  ;; either all `org-todo-keywords' before the "|" entry or all but the last keyword
  :custom
  (org-journal-dir my--org-journal-folder)
  (org-journal-file-format "%Y%m%d.org") ; add '.org' extension
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
    "Call `org-journal-new-entry' with a prefix to not create a new entry"
    (interactive)
    (org-journal-new-entry t))
  (defhydra hydra-journal ()
    ""
    ("n" org-journal-next-entry "next entry")
    ("N" org-journal-previous-entry "previous entry")
    ("q" nil "quit")))

;; quickly browse, filter and edit plain text notes
(use-package deft
  :general
  (my/leader-key
    :infix my/infix/org
    "s" '(deft :which-key "search notes"))
  :custom
  (deft-directory my--org-notes-folder)   ; specific the deft directory
  (deft-recursive t)                      ; also search in sub folders
  (deft-default-extension "org")          ; create ORG files by default
  (deft-use-filter-string-for-filename t) ; use the filter string for the filename
  (deft-auto-save-interval 0)             ; do not auto save files created by deft
  :config
  ;; make sure `deft' doesn't create filenames which contain spaces
  (defun my//deft-new-file-named-advice (args)
    "Filter the passed file name for `deft-new-file-named' by replacing all spaces with '-'."
    (list (s-replace " " "-" (car args))))
  (advice-add 'deft-new-file-named :filter-args #'my//deft-new-file-named-advice)

  (general-define-key
    :keymaps 'deft-mode-map
    :states 'normal
    "q" 'kill-current-buffer
    "C-g" 'kill-current-buffer)

  (general-define-key
    :keymaps 'deft-mode-map
    :states 'insert
    "C-g" 'kill-current-buffer
    "C-j" 'next-line
    "C-k" 'previous-line)

  ;; set default evil state for 'deft' to 'insert' so we can directly start typing
  (evil-set-initial-state 'deft-mode 'insert))

;;;** outline

;; organize source code or text in an org like manner
(use-package outline
  :ensure nil
  :defer t
  :config
  (defhydra hydra-outline-movement (:hint nil)
    "
^Movement^              ^Show/Hide^        ^Other^
^^^^^^---------------------------------------------------------
_j_: goto next          _S_: show all      _TAB_: fold cycle
_k_: goto previous      _H_: hide all      _z_: center
_J_: move down          _O_: hide others
_K_: move up
^^^^^^---------------------------------------------------------
[_q_]: quit
^^^^^^---------------------------------------------------------
"
    ("j" outline-next-visible-heading)
    ("k" outline-previous-visible-heading)
    ("J" outline-move-subtree-down)
    ("K" outline-move-subtree-up)
    ("S" outline-show-all)
    ("H" (lambda ()
	     (interactive)
	     (outline-hide-sublevels 4)))
    ("O" outline-hide-other)
    ("TAB" outline-cycle)
    ("z" evil-scroll-line-to-center)
    ("q" nil))

  (my/leader-key
    :infix my/infix/custom
    :keymaps 'outline-minor-mode-map
    "l" '(hydra-outline-movement/body :which-key "[outline]")))

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
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

;;;* miscellaneous

;; a collection of packages and functions which do not fit within another more specific category

;;;** packages

;; miscellaneous internal and external packages configurations

;;;*** internal

;; a grab-bag of basic emacs commands not specifically related to something else
(use-package simple
  :ensure nil
  :init
  (defvar my--visual-line-toggle nil
    "Indicator if the \"visual line mode\" with custom keybindings is enabled for the current buffer.")

  (defun my//visual-line-wk-replacement (entry)
    "Which key replacement function for the custom `visual-line-mode'."
    (let ((key (car entry)))
      (if my--visual-line-toggle
	`(,key . "[X] visual lines")
	`(,key . "[ ] visual lines"))))

  (defun my//truncate-lines-wk-replacement (entry)
    "Which key replacement function for `truncate-lines'."
    (let ((key (car entry)))
      (if truncate-lines
	`(,key . "[X] truncated lines")
	`(,key . "[ ] truncated lines"))))
  :general
  (my/leader-key
    "u" '(universal-argument :which-key "universal argument"))

  (my/leader-key
    :infix my/infix/toggle
    "l" '(my/toggle-visual-line :which-key my//visual-line-wk-replacement)
    "t" '(toggle-truncate-lines :which-key my//truncate-lines-wk-replacement))

  (my/leader-key
    :infix my/infix/buffer
    "a" '(mark-whole-buffer :which-key "select all content")
    "d" '(kill-current-buffer :which-key "kill")
    "n" '(my/new-empty-buffer :which-key "new"))

  (general-define-key
   :states '(normal insert)
    "C-M-<backspace>" 'delete-indentation
    "C-M-<backspace>" 'delete-indentation)
  :config
  (defun my/new-empty-buffer ()
    "Create a new empty buffer."
    (interactive)
    (let ((buffer (generate-new-buffer "untitled")))
      (switch-to-buffer buffer)  ; switch current window to new buffer
      (fundamental-mode)         ; set default major-mode to `fundamental-mode'
      (font-lock-mode -1)        ; disable `font-lock-mode' to prevent issues with large amounts of text
      (set-buffer-modified-p t)  ; mark buffer as modified, so we are able to save it as an emtpy file
      (setq buffer-offer-save t) ; ask if the buffer should be save when quitting emacs
      buffer))

  (defun my/toggle-visual-line ()
    "Toggle `visual-line-mode' and set custom keybindings for it."
    (interactive)
    (if my--visual-line-toggle
      ;; deactivate `visual-line-mode'
      (progn
	(visual-line-mode -1)
	(evil-normalize-keymaps))
      ;; activate `visual-line-mode'
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
    (setq-local my--visual-line-toggle (not my--visual-line-toggle))))

;; emacs's built-in help system
(use-package help
  :ensure nil
  ;; focus new help windows when opened
  :custom (help-window-select t)
  :config
  ;; key bindings for the most used help commands
  (my/leader-key
    :infix my/infix/help
    ;; the `counsel' variants highlight interactive functions & customizable variables
    "f" '(counsel-describe-function :which-key "describe function")
    "v" '(counsel-describe-variable :which-key "describe variable")
    "m" '(describe-mode :which-key "describe mode")
    "k" '(describe-key :which-key "describe key")))

;; visualize blanks (TAB, (HARD) SPACE & NEWLINE)
(use-package whitespace
  :ensure nil
  :init
  (defun my//whitespace-wk-replacement (entry)
    "Which key replacement function for `whitespace-mode'."
    (let ((key (car entry)))
      (if (bound-and-true-p whitespace-mode)
	`(,key . "[X] whitespaces")
	`(,key . "[ ] whitespaces"))))
  :general
  (my/leader-key
    :infix my/infix/toggle
    "w" '(whitespace-mode :which-key my//whitespace-wk-replacement)))

;; menu for install/remove/upgrade packages
(use-package package
  :ensure nil
  :config
  (my/leader-key
    :infix my/infix/custom
    "p" '(list-packages :which-key "package menu"))

  (my/major-mode-leader-key
    :keymaps 'package-menu-mode-map
    "" '(:ignore t :which-key "Package Menu")
    "f" '(package-menu-filter-by-name :which-key "filter by name")
    "c" '(package-menu-clear-filter :which-key "clear filters")))

;; display the visible line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode))
  :custom
  ;; count number of lines for the needed line-number width beforehand
  (display-line-numbers-width-start t)
  ;; don't shrink available space to prevent "flickering"
  (display-line-numbers-grow-only t)
  :init
  (defun my//display-line-numbers-wk-replacement (entry)
    "Which key replacement function for `display-line-numbers-mode'."
    (let ((key (car entry)))
      (if (bound-and-true-p display-line-numbers-mode)
	`(,key . "[X] line numbers")
	`(,key . "[ ] line numbers"))))
  :general
  (my/leader-key
    :infix my/infix/toggle
    "n" '(display-line-numbers-mode :which-key my//display-line-numbers-wk-replacement)))

;; highlight the current line
(use-package hl-line
  :ensure nil
  :config
  ;; highlight the current line by default
  (global-hl-line-mode)

  (defun my//global-hl-line-wk-replacement (entry)
    "Which key replacement function for the global hl-line mode."
    (let ((key (car entry)))
      (if (bound-and-true-p global-hl-line-mode)
	`(,key . "[X] line highlighting")
	`(,key . "[ ] line highlighting"))))

  (my/leader-key
    :infix my/infix/toggle
    "H" '(global-hl-line-mode :which-key my//global-hl-line-wk-replacement)))

;; move the mouse cursor out of the way
(use-package avoid
  :ensure nil
  :config (mouse-avoidance-mode 'cat-and-mouse))

(when (>= emacs-major-version 28)
  (use-package browse-url
    :custom
    ;; always use the "default" browsers for `browse-url-of-dired-file'
    ;; otherwise emacs tries to load the file into a buffer, which will not work for most non-text file types
    (browse-url-handlers '((".*" . browse-url-default-browser)))))

;;;*** external

;; restart emacs from within emacs
(use-package restart-emacs
  :general
  (my/leader-key
    :infix my/infix/quit
    "r" '(restart-emacs :which-key "restart")))

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

;; highlight "TODO" (and other) keywords
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; only collect garbage when idling
(use-package gcmh
  :init (gcmh-mode 1))

;; edit grep buffer and apply those changes to the corresponding file buffer
;; e.g. very useful in combination with `ivy-occur'
(use-package wgrep
  :defer t
  :config
  (my/major-mode-leader-key
    :keymaps 'wgrep-mode-map
    :major-modes '(nil)
    "c" '(wgrep-finish-edit :which-key "(wgrep) finish edit")
    "k" '(wgrep-abort-changes :which-key "(wgrep) cancel edit")))

;; simple, stable & linear undo/redo with emacs
(use-package undo-fu)

;; move lines/selected text up and down easily
(use-package drag-stuff
  :general
  ;; drag left/right only for visual state
  (general-define-key
   :states 'visual-state
   "C-S-h" 'drag-stuff-left
   "C-S-l" 'drag-stuff-right)

  (general-define-key
   :states '(normal insert visual)
   "C-S-j" 'drag-stuff-down
   "C-S-k" 'drag-stuff-up)

  :config (drag-stuff-mode t))

;; jump to text/words/lines
(use-package avy
  :general
  ;; make avy available as VIM operator
  (general-define-key
   :states 'operator
   "gw" 'evil-avy-goto-word-1
   "gl" 'evil-avy-goto-line)

  (my/leader-key
    :infix my/infix/jump
    "w" '(avy-goto-word-1 :which-key "goto word")
    "j" '(avy-goto-char-timer :which-key "goto char-seq")
    "l" '(avy-goto-line :which-key "goto line")))

;; quickly open a shell buffer from anywhere
(use-package shell-pop
  :commands shell-pop
  :hook (shell-pop-in-after . evil-insert-state)
  :custom
  (shell-pop-full-span t)
  ;; set default shell type to "ansi-term"
  (shell-pop-shell-type (quote ("ansi-term"
				"*ansi-term*"
				(lambda nil (ansi-term shell-pop-term-shell)))))
  :general (my/leader-key "'" '(shell-pop :which-key "shell")))

;; copy environment variables from your local shell to emacs
(use-package exec-path-from-shell
  :defer 3
  :config
  (exec-path-from-shell-initialize))

;; visual popup interface library for emacs
;; NOTE: needed by `google-translate'
(use-package popup
  :defer t)

;; this is the package which needs to be installed
;; in order for `google-translate-smooth-ui' to be available
(use-package google-translate
  :defer t)

;; emacs interface for google-translate
(use-package google-translate-smooth-ui
  :ensure nil
  :custom
  (google-translate-backend-method 'curl)
  :general
  (my/leader-key
    :infix my/infix/custom
    "G" '(google-translate-smooth-translate :which-key "google translate"))
  :config
  ;; To fix error: google-translate--search-tkk: Search failed: ",tkk:'"
  ;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-727920888
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

;;;** functions

;; several different utility functions

(defun my//byte-compile-init-dir ()
  "Force byte compilation of all .el files in the 'elpa' directory."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elpa/") 0))

(defun remove-all-text-properties (s)
  "Remove all text properties from S."
  (set-text-properties 0 (length s) nil s)
  s)

;; functions for interacting with Apache Tomcat

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

;;;* flycheck

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :general
  (my/leader-key
    :infix my/infix/toggle
    "f" '(flycheck-mode :which-key my//flycheck-which-key-replacement))
  (my/leader-key
    :keymaps 'flycheck-mode-map
    "e" '(hydra-flycheck/body :which-key "[errors]"))
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
  (defun my//flycheck-which-key-replacement (entry)
    "Which key replacement function for 'flycheck'."
    (let ((key (car entry)))
      (if (bound-and-true-p flycheck-mode)
	`(,key . "[X] flycheck")
	`(,key . "[ ] flycheck"))))
  :config
  (defhydra hydra-flycheck (:hint nil)
    "
^Navigation^         ^Misc^
^^^^--------------------------------
_n_: next error      _z_: center
_N_: previous error  _L_: error list
^^^^--------------------------------
[_q_]: quit
^^^^--------------------------------
"
    ("n" flycheck-next-error)
    ("N" flycheck-previous-error)
    ("L" flycheck-list-errors :exit t)
    ("z" evil-scroll-line-to-center)
    ("q" nil :color blue)))

;;;* programming

;;;** language server protocol (LSP)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-file-watchers nil)       ; disabe file watchers to prevent warning about too many files
  (lsp-headerline-breadcrumb-enable t) ; show breadcrumbs in headerline
  :init
  ;; increase the amount of data emacs reads from processes
  ;; some server responses are in the 800k - 3M range (emacs default: 4k)
  (setq read-process-output-max (* 3 1024 1024))

  (defmacro my/lsp-keybindings (keymap name &rest additional-bindings)
    "Set the default LSP keybindings for the given 'major-mode' KEYMAP.
Name the prefix for which-key according to the NAME string argument.
You can pass in ADDITIONAL-BINDINGS to add mode specific behavior or to overwrite unsupported pre-defined bindings."
    `(my/major-mode-leader-key
       :keymaps ,keymap
       "" '(:ignore t :which-key ,name)
       "a" '(lsp-execute-code-action :which-key "code action")
       "h" '(:ignore t :which-key "Help/Docs")
       "hh" '(lsp-ui-doc-glance :which-key "glance")
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
  (lsp-ui-sideline-enable nil)  ; disable side line view by default
  (lsp-ui-doc-position 'bottom) ; show the documentation frame at the bottom of the current window
  (lsp-ui-doc-enable nil))      ; disable doc mode by default

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
	;; check if `exec-path-from-shell' has run to ensure all env variables are present
	;; this is especially important to make the JS language server work with 'nvm'
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

;;;** markdown

(use-package markdown-mode
  :commands markdown-mode
  :custom (markdown-command "pandoc") ; use 'pandoc' to ensure GFM preview is working correctly
  :general
  (my/major-mode-leader-key
    :keymaps 'markdown-mode-map
    "" '(:ignore t :which-key "Markdown")
    "i" '(:ignore t :which-key "Insert")
    "il" '(markdown-insert-link :which-key "link")
    "ic" '(markdown-insert-gfm-code-block :which-key "code block")
    "ii" '(markdown-insert-image :which-key "image")
    "t" '(:ignore t :which-key "Table")
    "tn" '(markdown-insert-table :which-key "new table")
    "ta" '(markdown-table-align :which-key "align")
    "te" '(hydra-markdown-table/body :which-key "[edit]"))
  :config
  ;; cycle (tables) in normal state
  (general-define-key
   :keymaps 'markdown-mode-map
   :states 'normal
   "TAB" 'markdown-cycle)

  (defhydra hydra-markdown-table (:hint nil
				  :pre (when (not (markdown-table-at-point-p))
					 (user-error "Not at a table")))
    "
Movement      ^^^^^Rows^            ^Columns^
^^^^^^^^^----------------------------------------------
    ^_k_^         _rj_: move down   _ch_: move left
 _h_     _l_      _rk_: move up     _cl_: move right
    ^_j_^         _ri_: insert      _ci_: insert
              ^^^^_rd_: delete      _cd_: delete
^^^^^^^^^----------------------------------------------
[_q_]: quit
^^^^^^^^^----------------------------------------------
"
    ;; movement
    ("h" markdown-table-backward-cell)
    ("l" markdown-table-forward-cell)
    ("k" my/markdown-table-previous-row)
    ("j" my/markdown-table-next-row)
    ;; rows
    ("rj" markdown-table-move-row-down)
    ("rk" markdown-table-move-row-up)
    ("ri" markdown-table-insert-row)
    ("rd" markdown-table-delete-row)
    ;; columns
    ("ch" markdown-table-move-column-left)
    ("cl" markdown-table-move-column-right)
    ("ci" markdown-table-insert-column)
    ("cd" markdown-table-delete-column)
    ("q" nil))
  (defun my/markdown-table-next-row ()
    (interactive)
    (let ((pos (point)))
      (evil-next-line)
      (when (not (markdown-table-at-point-p))
	(goto-char pos)
	(user-error "Cannot move to next table row"))))
  (defun my/markdown-table-previous-row ()
    (interactive)
    (let ((pos (point)))
      (evil-previous-line)
      (when (not (markdown-table-at-point-p))
	(goto-char pos)
	(user-error "Cannot move to previous table row")))))

(use-package markdown-preview-mode
  :after markdown-mode
  :general
  (my/major-mode-leader-key
    :keymaps 'markdown-mode-map
    "p" '(markdown-preview-mode :which-key my//markdown-preview-mode-which-key-replacement)
    "P" '(:ignore t :which-key "Preview")
    "Po" '(markdown-preview-open-browser :which-key "open preview in browser")
    "Pc" '(markdown-preview-cleanup :which-key "cleanup"))
  :init
  (defun my//markdown-preview-mode-which-key-replacement (entry)
    "Which key replacement function that checks for 'markdown-preview-mode'."
    (let ((key (car entry)))
      (if (bound-and-true-p markdown-preview-mode)
	`(,key . "[X] preview mode")
	`(,key . "[ ] preview mode"))))
  :config
  ;; set custom stylesheets and javascript so the preview looks more like Github markdown [src: https://github.com/ancane/markdown-preview-mode/issues/29]
  (setq markdown-preview-stylesheets
	(list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
              "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"
	      "<style>
                .markdown-body {
                  box-sizing: border-box;
                  min-width: 200px;
                  max-width: 980px;
                  margin: 0 auto;
                  padding: 45px;
                }

                @media (max-width: 767px) {
                  .markdown-body {
                    padding: 15px;
                  }
                }
               </style>")
	markdown-preview-javascript
	(list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
	      "<script>
                $(document).on('mdContentChange', function() {
                  $('pre code').each(function(i, block) {
                    hljs.highlightBlock(block);
                  });
                });
               </script>")))

;;;** java

(use-package lsp-java
  :hook ((java-mode . lsp))
  :general
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
	  (switch-to-buffer-other-window "*lsp-log*") ; open the *lsp-log* buffer to see the outcome of the configuration update
	  (goto-char (point-max))))))                 ; move to the end of the log buffer in order to see the most recent log lines

  (my/leader-key
    :infix my/infix/custom
    "gr" '(my/java/gradle-refresh :which-key "refresh project")))

(use-package dap-java
  ;; `dap-java' is part of `dap-mode' but needs to be loaded extra
  :ensure nil
  :after (dap-mode lsp-java))

;; `gradlew' is a local package and needs to be installed manually
(when (not (package-installed-p 'gradlew))
  (package-install-file (concat user-emacs-directory "repos/gradlew/")))

;; execute gradle wrapper tasks with ease from emacs
(use-package gradlew
  :ensure nil
  :custom (gradlew/post-dispatch-fn 'evil-force-normal-state)
  :init
  (defun my//gradlew-execute-task-from-list-wk-replacement (entry)
    "Which key replacement function for `gradlew-execute-task-from-list' which checks for current prefix arg."
    (let ((key (car entry)))
      (cond
       ((and prefix-arg (numberp prefix-arg))     ; numeric prefix arg
	`(,key . "task list (refresh & config)"))
       (prefix-arg                                ; non-numeric prefix arg
	`(,key . "task list (cache & config)"))
       (t                                         ; no prefix arg
	`(,key . "task list (cache)")))))

  (defun my//gradlew-execute-task-wk-replacement (entry)
    "Which key replacement function for `gradlew-execute-task' which checks for current prefix arg."
    (let ((key (car entry)))
      (if prefix-arg
	`(,key . "execute task (config)")
	`(,key . "execute task"))))

  :general
  (my/leader-key
    :infix my/infix/custom
    "g" '(:ignore t :which-key "Gradlew")
    "gl" '(gradlew-execute-task-from-list :which-key my//gradlew-execute-task-from-list-wk-replacement)
    "gx" '(gradlew-execute-task :which-key my//gradlew-execute-task-wk-replacement)))


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
  ;; make sure node/npm is installed and present in the PATH for debugging
  (dap-dart-setup))
  
(use-package flutter
  :after lsp-dart
  :custom (flutter-sdk-path (my//get-value-from-config "flutter-sdk" ""))
  :general
  (my/major-mode-leader-key
    :keymaps 'dart-mode-map
    "R" '(flutter-run-or-hot-reload :which-key "flutter start/reload")))

;;;* the end

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
