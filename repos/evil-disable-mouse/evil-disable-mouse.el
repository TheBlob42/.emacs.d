;;; evil-disable-mouse.el --- disable the mouse while using evil  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2020 TheBlob42

;; Author: TheBlob42
;; Version: 0
;; Created: 17.10.2020

;; Keywords: evil, mouse

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Who needs a mouse/trackpad when you are already using VIM keybindings?
;;
;; Avoid unwanted cursor movements because your palm slightly touched the trackpad while typing.
;; Withstand the temptation to lift your fingers from the keys to move the mouse cursor.
;;
;; This package is inspired from the excellent `disable-mouse' package by Steve Purcell.
;;
;; It provides the global minor mode `evil-disable-mouse-global-mode' which uses suppresses all mouse
;; events by intercepting them and running `ignore' instead.
;;
;; Other than `disable-mouse' it uses the "override" map from `general' to intercept these
;; mouse events before being passed to the dedicated `evil' state maps. This also allows us
;; to enable and disable the mode from within Emacs without any restart. To disable it we simply
;; bind 'nil' to all mouse events bindings so that are passed through again and handled properly.

;;; Code:
(require 'dash)
(require 'evil)
(require 'general)

(defgroup evil-disable-mouse nil
  "Disable mouse commands for EVIL usage."
  :prefix "evil-disable-mouse-"
  :group 'mouse)

(defconst evil-disable-mouse--modifier-combos
  '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "M-C-S-"))

(defconst evil-disable-mouse--multipliers
  '("" "double-" "triple-"))

(defconst evil-disable-mouse--button-events
  '("mouse" "up-mouse" "down-mouse" "drag-mouse"))

(defconst evil-disable-mouse--button-numbers
  '(1 2 3 4 5))

(defconst evil-disable-mouse--mouse-button-bindings
  (-flatten
   (-map (lambda (event)
	   (-map (-partial 'format "%s-%d" event) evil-disable-mouse--button-numbers))
	 evil-disable-mouse--button-events)))

(defun evil-disable-mouse--all-bindings ()
  "Combine all possible keybindings that involve a mouse key."
  (let ((bindings))
    (dolist (modifier evil-disable-mouse--modifier-combos)
      (dolist (multiplier evil-disable-mouse--multipliers)
	(dolist (binding evil-disable-mouse--mouse-button-bindings)
	  (push (concat "<" modifier multiplier binding ">") bindings))))
    bindings))

(defun evil-disable-mouse--intercept-mouse-bindings (keymap intercept)
  "(De)Activate the interception of mouse keybindings in KEYMAP.
If INTERCEPT is non-nil intercept all mouse bindings,
otherwise remove the interception.

This function binds either `ignore' or nil to all mouse related
bindings in KEYMAP, so all present mouse bindings within it
are 'destroyed'.
Instead use KEYMAP just a layer on top of your actual keymap which handles
mouse bindings. In case of `evil-disable-mouse' we use the 'override and
'local keymaps provided by `general' to achive this."
  (let* (;; to intercept all mouse bindings map them to 'ignore
	 ;; use nil to let them pass through an be handled by an underlying keymap
	 (cmd (when intercept
		(quote 'ignore)))
	 ;; build the pairs of mouse event plus cmd (for later macro usage)
	 ;; e.g.("<mouse-1>" 'ignore "<mouse-2>" 'ignore ...)
	 (binding-pairs (-interleave (evil-disable-mouse--all-bindings)
				     (-cycle (list cmd)))))
    (eval `(general-define-key
	    :keymaps (quote ,keymap)
	    :states '(normal insert visual motion operator emacs)
	    ,@binding-pairs))))

;;;###autoload
(define-minor-mode evil-disable-mouse-local-mode
  "Disable the mouse locally.
You can still use the mouse to interact with GUI elements such as the divider lines."
  nil
  :require evil-disable-mouse
  :global nil
  (evil-disable-mouse--intercept-mouse-bindings 'local
						evil-disable-mouse-local-mode))

;;;###autoload
(define-minor-mode evil-disable-mouse-global-mode
  "Disable the mouse globally.
You can still use the mouse to interact with GUI elements such as the divider lines."
  nil
  :require 'evil-disable-mouse
  :global t
  (evil-disable-mouse--intercept-mouse-bindings 'override
						evil-disable-mouse-global-mode))

(provide 'evil-disable-mouse)
;;; evil-disable-mouse.el ends here
