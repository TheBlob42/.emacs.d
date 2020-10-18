;;; evil-disable-mouse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-disable-mouse" "evil-disable-mouse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-disable-mouse.el

(autoload 'evil-disable-mouse-local-mode "evil-disable-mouse" "\
Disable the mouse locally.
You can still use the mouse to interact with GUI elements such as the divider lines.

If called interactively, enable Evil-Disable-Mouse-Local mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar evil-disable-mouse-global-mode nil "\
Non-nil if Evil-Disable-Mouse-Global mode is enabled.
See the `evil-disable-mouse-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-disable-mouse-global-mode'.")

(custom-autoload 'evil-disable-mouse-global-mode "evil-disable-mouse" nil)

(autoload 'evil-disable-mouse-global-mode "evil-disable-mouse" "\
Disable the mouse globally.
You can still use the mouse to interact with GUI elements such as the divider lines.

If called interactively, enable Evil-Disable-Mouse-Global mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-disable-mouse" '("evil-disable-mouse--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-disable-mouse-autoloads.el ends here
