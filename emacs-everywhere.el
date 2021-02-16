;;; emacs-everywhere.el --- System-wide popup windows for quick edits -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TEC

;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: February 06, 2021
;; Modified: February 06, 2021
;; Version: 0.0.1
;; Keywords: conenience, frames
;; Homepage: https://github.com/tec/emacs-everywhere
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
  '(emacs-everywhere-set-frame-name
    emacs-everywhere-set-frame-position
    emacs-everywhere-insert-selection
    emacs-everywhere-remove-trailing-whitespace
    (cond
     ((executable-find "pandoc") #'org-mode)
     ((fboundp 'markdown-mode) #'emacs-everywhere-major-mode-org-or-markdown)
     (t #'text-mode))
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

;; Semi-internal variables

(defvar-local emacs-everywhere-current-app nil
  "The current `emacs-everywhere-app'")
;; Prevents buffer-local variable from being unset by major mode changes
(put 'emacs-everywhere-current-app 'permanent-local t)

;; Make the byte-compiler happier

(declare-function org-in-src-block-p "org")
(declare-function org-ctrl-c-ctrl-c "org")
(declare-function org-export-to-buffer "ox")
(declare-function evil-insert-state "evil-states")
(declare-function spell-fu-buffer "spell-fu")
(declare-function markdown-mode "markdown-mode")

;;; Primary functionality

;;;###autoload
(defun emacs-everywhere ()
  "Lanuch the emacs-everywhere frame from emacsclient."
  (call-process "emacsclient" nil 0 nil
                "-c" "-F" (prin1-to-string emacs-everywhere-frame-parameters)
                "--eval" (prin1-to-string
                          `(emacs-everywhere-initialise
                            ,(emacs-everywhere-app-info)))))

(defun emacs-everywhere-initialise (&optional app)
  "Entry point for the executable.
APP is an `emacs-everywhere-app' struct."
  (switch-to-buffer (generate-new-buffer "*Emacs Everywhere*"))
  (let ((window (or app (emacs-everywhere-app-info))))
    (setq-local emacs-everywhere-current-app window)
    (with-demoted-errors "Emacs Everywhere: error running init hooks, %s"
      (run-hooks 'emacs-everywhere-init-hooks))
    (emacs-everywhere-mode 1)
    (select-frame-set-input-focus (selected-frame))))

(define-minor-mode emacs-everywhere-mode
  "Tweak the current buffer to add some emacs-everywhere considerations."
  :init-value nil
  :keymap (list
           ;; Finish edit, but be smart in org mode
           (cons (kbd "C-c C-c")
                 (lambda ()
                   (interactive)
                   (if (and (eq major-mode 'org-mode)
                            (org-in-src-block-p))
                       (org-ctrl-c-ctrl-c)
                     (emacs-everywhere-finish))))
           ;; Kill frame
           (cons (kbd "C-x 5 0")
                 (lambda ()
                   (interactive)
                   (emacs-everywhere-finish)))
           ;; Abort edit. emacs-anywhere saves the current edit for next time.
           (cons (kbd "C-c C-k")
                 (lambda ()
                   (interactive)
                   (emacs-everywhere-finish t))))
  ;; line breaking
  (turn-off-auto-fill)
  (visual-line-mode t)
  ;; DEL/C-SPC to clear (first keystroke only)
  (set-transient-map (let ((keymap (make-sparse-keymap)))
                       (define-key keymap (kbd "DEL")
                         (lambda ()
                           (interactive)
                           (delete-region (point-min) (point-max))))
                       (define-key keymap (kbd "C-SPC")
                         (lambda ()
                           (interactive)
                           (delete-region (point-min) (point-max))))
                       keymap)))

(defun emacs-everywhere-finish (&optional abort)
  "Copy buffer content, close emacs-everywhere window, and maybe paste.
Must only be called within a emacs-everywhere buffer.
Never paste content when ABORT is non-nil."
  (run-hooks 'emacs-everywhere-final-hooks)
  (gui-select-text (buffer-string))
  (unless (eq system-type 'darwin) ; handle clipboard finicklyness
    (let ((clip-file (make-temp-file "ee-clipboard"))
          (inhibit-message t)
          (require-final-newline nil)
          write-file-functions)
      (pp (buffer-string))
      (write-file clip-file)
      (call-process "xclip" nil nil nil "-selection" "clipboard" clip-file)))
  (sit-for 0.01) ; prevents weird multi-second pause, lets clipboard info propagate
  (let ((window-id (emacs-everywhere-app-id emacs-everywhere-current-app)))
    (if (eq system-type 'darwin)
        (call-process "osascript" nil nil nil
                      "-e" (format "tell application \"%s\" to activate" window-id))
      (call-process "xdotool" nil nil nil
                    "windowactivate" "--sync" (number-to-string window-id)))
    (when (and emacs-everywhere-paste-p (not abort))
      (if (eq system-type 'darwin)
          (call-process "osascript" nil nil nil
                        "-e" "tell application \"System Events\" to keystroke (the clipboard as text)")
        (call-process "xdotool" nil nil nil
                      "key" "--window" (number-to-string window-id) "--clearmodifiers" "Shift+Insert"))))
  (kill-buffer (current-buffer))
  (delete-frame))

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
        (sit-for 0.01) ; lets clipboard info propagate
        (yank))
    (when-let ((selection (gui-get-selection 'PRIMARY)))
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

(defun emacs-everywhere-return-converted-org-to-gfm ()
  "When appropriate, convert org buffer to markdown."
  (when (and (eq major-mode 'org-mode)
             (emacs-everywhere-markdown-p))
    (goto-char (point-min))
    (insert "#+property: header-args :exports both\n#+options: toc:nil\n")
    (let ((export-buffer (generate-new-buffer "*Emacs Everywhere Export*")))
      (org-export-to-buffer (if (featurep 'ox-gfm) 'gfm 'md) export-buffer)
      (delete-window)
      (erase-buffer)
      (insert-buffer-substring export-buffer)
      (kill-buffer export-buffer))))

(provide 'emacs-everywhere)
;;; emacs-everywhere.el ends here
