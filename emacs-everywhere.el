;;; emacs-everywhere.el --- System-wide popup windows for quick edits -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TEC

;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: February 06, 2021
;; Modified: February 06, 2021
;; Version: 0.0.1
;; Keywords: conenience, frames
;; Homepage: https://github.com/tecosaur/emacs-everywhere
;; Package-Requires: ((emacs "26.3") (cl-lib "0.5"))

;;; License:

;; This file is part of org-pandoc-import, which is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  System-wide popup Emacs windows for quick edits

;;; Code:

(require 'cl-lib)

(defgroup emacs-everywhere ()
  "Customise group for Emacs-everywhere."
  :group 'convenience)

(defcustom emacs-everywhere-paste-p t
  "Whether to paste the final buffer content on exit."
  :type 'boolean
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-markdown-windows
  '("Stack Exchange" "Stack Overflow" "Reddit" ; Sites
    "Pull Request" "Issue" "Comparing .*\\.\\.\\." ; Github
    "Discord")
  "For use with `emacs-everywhere-markdown-p'.
Patterns which are matched against the window title."
  :type '(rep string)
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-markdown-apps
  '("Discord")
  "For use with `emacs-everywhere-markdown-p'.
Patterns which are matched against the app name."
  :type '(rep string)
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-frame-name-format "Emacs Everywhere :: %s — %s"
  "Format string used to produce the frame name.
Formatted with the app name, and truncated window name."
  :type 'string
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-init-hooks
  `(emacs-everywhere-set-frame-name
    emacs-everywhere-set-frame-position
    ,(cond
      ((executable-find "pandoc") #'org-mode)
      ((fboundp 'markdown-mode) #'emacs-everywhere-major-mode-org-or-markdown)
      (t #'text-mode))
    emacs-everywhere-insert-selection
    emacs-everywhere-remove-trailing-whitespace
    emacs-everywhere-init-spell-check)
  "Hooks to be run before function `emacs-everywhere-mode'."
  :type 'hook
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-final-hooks
  '(emacs-everywhere-remove-trailing-whitespace
    emacs-everywhere-return-converted-org-to-gfm)
  "Hooks to be run just before content is copied."
  :type 'hook
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-frame-parameters
  `((name . "emacs-everywhere")
    (width . 80)
    (height . 12))
  "Parameters `make-frame' recognises to apply to the emacs-everywhere frame."
  :type 'list
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-file-patterns
  (let ((default-directory temporary-file-directory))
    (list (concat "^" (regexp-quote (file-truename "emacs-everywhere-")))
          ;; For qutebrowser 'editor.command' support
          (concat "^" (regexp-quote (file-truename "qutebrowser-editor-")))))
  "A list of file regexps to activate `emacs-everywhere-mode' for."
  :type '(repeat regexp)
  :group 'emacs-everywhere)

;; Semi-internal variables

(defvar-local emacs-everywhere-current-app nil
  "The current `emacs-everywhere-app'")
;; Prevents buffer-local variable from being unset by major mode changes
(put 'emacs-everywhere-current-app 'permanent-local t)

(defvar-local emacs-everywhere--contents nil)

;; Make the byte-compiler happier

(declare-function org-in-src-block-p "org")
(declare-function org-ctrl-c-ctrl-c "org")
(declare-function org-export-to-buffer "ox")
(declare-function evil-insert-state "evil-states")
(declare-function spell-fu-buffer "spell-fu")
(declare-function markdown-mode "markdown-mode")

;;; Primary functionality

;;;###autoload
(defun emacs-everywhere (&optional file line column)
  "Lanuch the emacs-everywhere frame from emacsclient."
  (apply #'call-process "emacsclient" nil 0 nil
         (delq
          nil (list
               "-c" "-F"
               (prin1-to-string
                (cons (cons 'emacs-everywhere-app (emacs-everywhere-app-info))
                      emacs-everywhere-frame-parameters))
               (cond ((and line column) (format "+%d:%d" line column))
                     (line              (format "+%d" line)))
               (or file (make-temp-file "emacs-everywhere-"))))))

(defun emacs-everywhere-file-p (file)
  "Return non-nil if FILE should be handled by emacs-everywhere.
This matches FILE against `emacs-everywhere-file-patterns'."
  (let ((file (file-truename file)))
    (cl-some (lambda (pattern) (string-match-p pattern file))
             emacs-everywhere-file-patterns)))

;;;###autoload
(defun emacs-everywhere-initialise ()
  "Entry point for the executable.
APP is an `emacs-everywhere-app' struct."
  (let ((file (buffer-file-name (buffer-base-buffer))))
   (when (and file (emacs-everywhere-file-p file))
    (let ((app (or (frame-parameter nil 'emacs-everywhere-app)
                   (emacs-everywhere-app-info))))
      (setq-local emacs-everywhere-current-app app)
      (with-demoted-errors "Emacs Everywhere: error running init hooks, %s"
        (run-hooks 'emacs-everywhere-init-hooks))
      (emacs-everywhere-mode 1)
      (setq emacs-everywhere--contents (buffer-string))))))

;;;###autoload
(add-hook 'server-visit-hook #'emacs-everywhere-initialise)
(add-hook 'server-done-hook #'emacs-everywhere-finish)

(defvar emacs-everywhere-mode-initial-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "DEL") #'emacs-everywhere-erase-buffer)
    (define-key keymap (kbd "C-SPC") #'emacs-everywhere-erase-buffer)
    keymap)
  "Transient keymap invoked when an emacs-everywhere buffer is first created.
Set to `nil' to prevent this transient map from activating in emacs-everywhere
buffers.")

(define-minor-mode emacs-everywhere-mode
  "Tweak the current buffer to add some emacs-everywhere considerations."
  :init-value nil
  :keymap `((,(kbd "C-c C-c") . emacs-everywhere-finish-or-ctrl-c-ctrl-c)
            (,(kbd "C-x 5 0") . emacs-everywhere-finish)
            (,(kbd "C-c C-k") . emacs-everywhere-abort))
  ;; line breaking
  (turn-off-auto-fill)
  (visual-line-mode t)
  ;; DEL/C-SPC to clear (first keystroke only)
  (when (keymapp emacs-everywhere-mode-initial-map)
    (set-transient-map emacs-everywhere-mode-initial-map)))

(defun emacs-everywhere-erase-buffer ()
  "Delete the contents of the current buffer."
  (interactive)
  (delete-region (point-min) (point-max)))

(defun emacs-everywhere-finish-or-ctrl-c-ctrl-c ()
  "Finish emacs-everywhere session or invoke `org-ctrl-c-ctrl-c' in org-mode."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (org-in-src-block-p))
      (org-ctrl-c-ctrl-c)
    (emacs-everywhere-finish)))

(defun emacs-everywhere-finish (&optional abort)
  "Copy buffer content, close emacs-everywhere window, and maybe paste.
Must only be called within a emacs-everywhere buffer.
Never paste content when ABORT is non-nil."
  (interactive)
  (when emacs-everywhere-mode
    (when (equal emacs-everywhere--contents (buffer-string))
      (setq abort t))
    (unless abort
      (run-hooks 'emacs-everywhere-final-hooks)
      (gui-select-text (buffer-string))
      (unless (eq system-type 'darwin) ; handle clipboard finicklyness
        (let ((inhibit-message t)
              (require-final-newline nil)
              write-file-functions)
          (write-file buffer-file-name)
          (pp (buffer-string))
          (call-process "xclip" nil nil nil "-selection" "clipboard" buffer-file-name))))
    (sleep-for 0.01) ; prevents weird multi-second pause, lets clipboard info propagate
    (let ((window-id (emacs-everywhere-app-id emacs-everywhere-current-app)))
      (if (eq system-type 'darwin)
          (call-process "osascript" nil nil nil
                        "-e" (format "tell application \"%s\" to activate" window-id))
        (call-process "xdotool" nil nil nil
                      "windowactivate" "--sync" (number-to-string window-id)))
      ;; The frame only has this parameter if this package initialized the temp
      ;; file its displaying. Otherwise, it was created by another program, likely
      ;; a browser with direct EDITOR support, like qutebrowser.
      (when (and (frame-parameter nil 'emacs-everywhere-app)
                 emacs-everywhere-paste-p
                 (not abort))
        (if (eq system-type 'darwin)
            (call-process "osascript" nil nil nil
                          "-e" "tell application \"System Events\" to keystroke \"v\" using command down")
          (call-process "xdotool" nil nil nil
                        "key" "--clearmodifiers" "Shift+Insert"))))
    ;; Clean up after ourselves in case the buffer survives `server-buffer-done'
    ;; (b/c `server-existing-buffer' is non-nil).
    (emacs-everywhere-mode -1)
    (server-buffer-done (current-buffer))))

(defun emacs-everywhere-abort ()
  "Abort current emacs-everywhere session."
  (interactive)
  (emacs-everywhere-finish t))

;;; Window info

(cl-defstruct emacs-everywhere-app
  "Metadata about the last focused window before emacs-everywhere was invoked."
  id class title geometry)

(defun emacs-everywhere-app-info ()
  "Return information on the active window."
  (let ((w (pcase system-type
             (`darwin (emacs-everywhere-app-info-osx))
             (_ (emacs-everywhere-app-info-linux)))))
    (setf (emacs-everywhere-app-title w)
          (replace-regexp-in-string
           (format " ?-[A-Za-z0-9 ]*%s"
                   (regexp-quote (emacs-everywhere-app-class w)))
           ""
           (replace-regexp-in-string
            "[^[:ascii:]]+" "-" (emacs-everywhere-app-title w))))
    w))

(defun emacs-everywhere-call (command &rest args)
  "Execute COMMAND with ARGS synchronously."
  (with-temp-buffer
    (apply #'call-process command nil t nil (remq nil args))
    (string-trim (buffer-string))))

(defun emacs-everywhere-app-info-linux ()
  "Return information on the active window, on linux."
  (let ((window-id (emacs-everywhere-call "xdotool" "getactivewindow")))
    (let ((app-name
           (car (split-string-and-unquote
                 (string-trim-left
                  (emacs-everywhere-call "xprop" "-id" window-id "WM_CLASS")
                  "[^ ]+ = \"[^\"]+\", "))))
          (window-title
           (car (split-string-and-unquote
                 (string-trim-left
                  (emacs-everywhere-call "xprop" "-id" window-id "_NET_WM_NAME")
                  "[^ ]+ = "))))
          (window-geometry
           (let ((info (mapcar (lambda (line)
                                 (split-string line ":" nil "[ \t]+"))
                               (split-string
                                (emacs-everywhere-call "xwininfo" "-id" window-id) "\n"))))
             (mapcar #'string-to-number
                     (list (cadr (assoc "Absolute upper-left X" info))
                           (cadr (assoc "Absolute upper-left Y" info))
                           (cadr (assoc "Relative upper-left X" info))
                           (cadr (assoc "Relative upper-left Y" info))
                           (cadr (assoc "Width" info))
                           (cadr (assoc "Height" info)))))))
      (make-emacs-everywhere-app
       :id (string-to-number window-id)
       :class app-name
       :title window-title
       :geometry (list
                  (if (= (nth 0 window-geometry) (nth 2 window-geometry))
                      (nth 0 window-geometry)
                    (- (nth 0 window-geometry) (nth 2 window-geometry)))
                  (if (= (nth 1 window-geometry) (nth 3 window-geometry))
                      (nth 1 window-geometry)
                    (- (nth 1 window-geometry) (nth 3 window-geometry)))
                  (nth 4 window-geometry)
                  (nth 5 window-geometry))))))

(defvar emacs-everywhere--dir (file-name-directory load-file-name))

(defun emacs-everywhere-app-info-osx ()
  "Return information on the active window, on osx."
  (emacs-everywhere-ensure-oscascript-compiled)
  (let ((default-directory emacs-everywhere--dir))
    (let ((app-name (emacs-everywhere-call
                     "osascript" "app-name"))
          (window-title (emacs-everywhere-call
                         "osascript" "window-title"))
          (window-geometry (mapcar #'string-to-number
                                   (split-string
                                    (emacs-everywhere-call
                                     "osascript" "window-geometry") ", "))))
      (make-emacs-everywhere-app
       :id app-name
       :class app-name
       :title window-title
       :geometry window-geometry))))

(defun emacs-everywhere-ensure-oscascript-compiled (&optional force)
  "Ensure that compiled oscascript files are present.
Will always compile when FORCE is non-nil."
  (unless (and (file-exists-p "app-name")
               (file-exists-p "window-geometry")
               (file-exists-p "window-title")
               (not force))
    (let ((default-directory emacs-everywhere--dir)
          (app-name
           "tell application \"System Events\"
    set frontAppName to name of first application process whose frontmost is true
end tell
return frontAppName")
          (window-geometry
           "tell application \"System Events\"
     set frontWindow to front window of (first application process whose frontmost is true)
     set windowPosition to (get position of frontWindow)
     set windowSize to (get size of frontWindow)
end tell
return windowPosition & windowSize")
          (window-title
           "set windowTitle to \"\"
tell application \"System Events\"
     set frontAppProcess to first application process whose frontmost is true
end tell
tell frontAppProcess
    if count of windows > 0 then
        set windowTitle to name of front window
    end if
end tell
return windowTitle"))
      (dolist (script `(("app-name" . ,app-name)
                        ("window-geometry" . ,window-geometry)
                        ("window-title" . ,window-title)))
        (write-region (cdr script) nil (concat (car script) ".applescript"))
        (shell-command (format "osacompile -r scpt:128 -t osas -o %s %s"
                               (car script) (concat (car script) ".applescript")))))))

;;; Secondary functionality

(defun emacs-everywhere-set-frame-name ()
  "Set the frame name based on `emacs-everywhere-frame-name-format'."
  (set-frame-name
   (format emacs-everywhere-frame-name-format
           (emacs-everywhere-app-class emacs-everywhere-current-app)
           (truncate-string-to-width
            (emacs-everywhere-app-title emacs-everywhere-current-app)
            45 nil nil "…"))))

(defun emacs-everywhere-remove-trailing-whitespace ()
  "Move point to the end of the buffer, and remove all trailing whitespace."
  (goto-char (max-char))
  (delete-trailing-whitespace)
  (delete-char (- (skip-chars-backward "\n"))))

(defun emacs-everywhere-set-frame-position ()
  "Set the size and position of the emacs-everywhere frame."
  (cl-destructuring-bind (x . y) (mouse-absolute-pixel-position)
    (set-frame-position (selected-frame)
                        (- x 100)
                        (- y 50))))

(defun emacs-everywhere-insert-selection ()
  "Insert the last text selection into the buffer."
  (if (eq system-type 'darwin)
      (progn
        (call-process "osascript" nil nil nil
                      "-e" "tell application \"System Events\" to keystroke \"c\" using command down")
        (sleep-for 0.01) ; lets clipboard info propagate
        (yank))
    (when-let ((selection (gui-get-selection 'PRIMARY 'UTF8_STRING)))
      (gui-backend-set-selection 'PRIMARY "")
      (insert selection)))
  (when (and (eq major-mode 'org-mode)
             (emacs-everywhere-markdown-p)
             (executable-find "pandoc"))
    (shell-command-on-region (point-min) (point-max)
                             "pandoc -f markdown-auto_identifiers -t org" nil t)
    (deactivate-mark) (goto-char (point-max)))
  (cond ((bound-and-true-p evil-local-mode) (evil-insert-state))))

(defun emacs-everywhere-init-spell-check ()
  "Run a spell check function on the buffer, using a relevant enabled mode."
  (cond ((bound-and-true-p spell-fu-mode) (spell-fu-buffer))
        ((bound-and-true-p flyspell-mode) (flyspell-buffer))))

(defun emacs-everywhere-markdown-p ()
  "Return t if the original window is recognised as markdown-flavoured."
  (let ((title (emacs-everywhere-app-title emacs-everywhere-current-app))
        (class (emacs-everywhere-app-class emacs-everywhere-current-app)))
    (or (cl-some (lambda (pattern)
                   (string-match-p pattern title))
                 emacs-everywhere-markdown-windows)
        (cl-some (lambda (pattern)
                   (string-match-p pattern class))
                 emacs-everywhere-markdown-apps))))

(defun emacs-everywhere-major-mode-org-or-markdown ()
  "Use markdow-mode, when window is recognised as markdown-flavoured.
Otherwise use `org-mode'."
  (if (emacs-everywhere-markdown-p)
      (markdown-mode)
    (org-mode)))

(defcustom emacs-everywhere-org-export-options
  "#+property: header-args :exports both
#+options: toc:nil\n"
  "A string inserted at the top of the Org buffer prior to export.
This is with the purpose of setting #+property and #+options parameters.

Should end in a newline to avoid interfering with the buffer content."
  :type 'string
  :group 'emacs-everywhere)

(defvar org-export-show-temporary-export-buffer)
(defun emacs-everywhere-return-converted-org-to-gfm ()
  "When appropriate, convert org buffer to markdown."
  (when (and (eq major-mode 'org-mode)
             (emacs-everywhere-markdown-p))
    (goto-char (point-min))
    (insert emacs-everywhere-org-export-options)
    (let (org-export-show-temporary-export-buffer)
      (org-export-to-buffer (if (featurep 'ox-gfm) 'gfm 'md) (current-buffer)))))

(provide 'emacs-everywhere)
;;; emacs-everywhere.el ends here
