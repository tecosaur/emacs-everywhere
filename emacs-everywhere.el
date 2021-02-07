;;; emacs-everywhere.el --- System-wide popup windows for quick edits -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TEC

;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: February 06, 2021
;; Modified: February 06, 2021
;; Version: 0.0.1
;; Keywords: conenience, frames
;; Homepage: https://github.com/tec/emacs-everywhere
;; Package-Requires: ((emacs "26.3"))

;;; License:

;; This file is part of org-pandoc-import, which is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  System-wide popup Emacs windows for quick edits

;;; Code:

(defgroup emacs-everywhere ()
  "Customise group for Emacs-everywhere."
  :group 'convenience)

(defcustom emacs-everywhere-paste-p t
  "Whether to paste the final buffer content on exit."
  :type 'boolean
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-major-mode-function
  #'emacs-everywhere-major-mode-org-or-markdown
  "Function which sets the major mode for the Emacs Everywhere buffer.

When set to `org-mode', pandoc is used to convert from markdown to Org
when applicable."
  :type 'function
  :options '(emacs-everywhere-major-mode-org-or-markdown
             org-mode)
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

(defcustom emacs-everywhere-init-hooks nil
  "Hooks to be run before function `emacs-everywhere-mode'."
  :type 'hook
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-final-hooks nil
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

(defvar-local emacs-everywhere-app-name nil
  "Name of the App which the original window is a instance of.")
(defvar-local emacs-everywhere-window-id nil
  "System ID of the original window.")
(defvar-local emacs-everywhere-window-title nil
  "Name (title) of the original window.")
(defvar-local emacs-everywhere-window-x nil
  "Leftmost pixel of the original window.")
(defvar-local emacs-everywhere-window-y nil
  "Top pixel of the original window.")
(defvar-local emacs-everywhere-window-width nil
  "Width of the original window.")
(defvar-local emacs-everywhere-window-height nil
  "Height of the original window.")
(defvar-local emacs-everywhere-mouse-x nil
  "Mouse X-coordiate at invocation.")
(defvar-local emacs-everywhere-mouse-y nil
  "Mouse Y-coordiate at invocation.")

;;; Primary functionality

;;;###autoload
(defun emacs-everywhere ()
  "Lanuch the emacs-everywhere frame from emacsclient."
  (call-process "emacsclient" nil 0 nil
                "-c" "-F" (prin1-to-string emacs-everywhere-frame-parameters)
                "--eval" (prin1-to-string
                          `(emacs-everywhere-initialise
                            ,@(emacs-everywhere-window-info)))))

(defun emacs-everywhere-initialise (app-name window-id window-title window-x window-y window-width window-height)
  "Entry point for the executable.
Provides: APP-NAME, WINDOW-ID, WINDOW-TITLE, WINDOW-X, WINDOW-Y,
          WINDOW-WIDTH, WINDOW-HEIGHT."
  (switch-to-buffer (generate-new-buffer "*Emacs Everywhere*"))
  (setq window-title
        (replace-regexp-in-string
         (format " ?-[A-Za-z0-9 ]*%s"
                 (regexp-quote app-name))
         ""
         (replace-regexp-in-string "[^[:ascii:]]+" "-" window-title)))
  (when emacs-everywhere-major-mode-function
    ;; Only set vars that may reasonably be used,
    ;; as they are (likely) about to be cleared.
    (setq-local emacs-everywhere-app-name app-name
                emacs-everywhere-window-id window-id
                emacs-everywhere-window-title window-title)
    (funcall emacs-everywhere-major-mode-function))
  (cl-destructuring-bind (mouse-x . mouse-y) (mouse-absolute-pixel-position)
    (setq-local emacs-everywhere-app-name app-name
                emacs-everywhere-window-id window-id
                emacs-everywhere-window-title window-title
                emacs-everywhere-window-x window-x
                emacs-everywhere-window-y window-y
                emacs-everywhere-window-width window-width
                emacs-everywhere-window-height window-height
                emacs-everywhere-mouse-x mouse-x
                emacs-everywhere-mouse-y mouse-y))
  (condition-case err
      (run-hooks 'emacs-everywhere-init-hooks)
    (error (message "Emacs Everywhere: error running init hooks, %s"
                    (error-message-string err))))
  (emacs-everywhere-mode 1)
  (select-frame-set-input-focus (selected-frame)))

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
  (if (eq system-type 'darwin)
      (call-process "osascript" nil nil nil
                    "-e" (format "tell application \"%s\" to activate" emacs-everywhere-app-name))
    (call-process "xdotool" nil nil nil
                  "windowactivate" "--sync" (number-to-string emacs-everywhere-window-id)))
  (when (and emacs-everywhere-paste-p (not abort))
    (if (eq system-type 'darwin)
        (call-process "osascript" nil nil nil
                      "-e" "tell application \"System Events\" to keystroke (the clipboard as text)")
      (call-process "xdotool" nil nil nil
                    "key" "--clearmodifiers" "Shift+Insert")))
  (kill-buffer (current-buffer))
  (delete-frame))

;;; Window info

(defun emacs-everywhere-window-info ()
  "Return information on the active window."
  (pcase system-type
    (`darwin (emacs-everywhere-window-info-osx))
    (_ (emacs-everywhere-window-info-linux))))

(defun emacs-everywhere-call (command &rest args)
  "Execute COMMAND with ARGS synchronously."
  (with-temp-buffer
    (apply #'call-process command nil t nil (remq nil args))
    (string-trim (buffer-string))))

(defun emacs-everywhere-window-info-linux ()
  "Return information on the active window, on linux."
  (let ((window-id (emacs-everywhere-call "xdotool" "getactivewindow")))
    (let ((app-name (car (split-string-and-unquote
                          (string-trim-left
                           (emacs-everywhere-call "xprop" "-id" window-id "WM_CLASS")
                           "[^ ]+ = \"[^\"]+\", "))))
          (window-title (car (split-string-and-unquote
                              (string-trim-left
                               (emacs-everywhere-call "xprop" "-id" window-id "_NET_WM_NAME")
                               "[^ ]+ = "))))
          (window-geometry (let ((info (mapcar (lambda (line)
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
      (list app-name
            (string-to-number window-id)
            window-title
            (if (= (nth 0 window-geometry) (nth 2 window-geometry))
                (nth 0 window-geometry)
              (- (nth 0 window-geometry) (nth 2 window-geometry)))
            (if (= (nth 1 window-geometry) (nth 3 window-geometry))
                (nth 1 window-geometry)
              (- (nth 1 window-geometry) (nth 3 window-geometry)))
            (nth 4 window-geometry)
            (nth 5 window-geometry)))))


(defun emacs-everywhere-window-info-osx ()
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
      (list app-name
            nil
            window-title
            (nth 0 window-geometry)
            (nth 1 window-geometry)
            (nth 2 window-geometry)
            (nth 3 window-geometry)))))

(defvar emacs-everywhere--dir (file-name-directory load-file-name))

(defun emacs-everywhere-ensure-oscascript-compiled (&optional force)
  "Ensure that compiled oscascript files are present."
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
           emacs-everywhere-app-name
           (truncate-string-to-width emacs-everywhere-window-title
                                     45 nil nil "…"))))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-name)

(defun emacs-everywhere-remove-trailing-whitespace ()
  "Move point to the end of the buffer, and remove all trailing whitespace."
  (goto-char (max-char))
  (delete-trailing-whitespace)
  (delete-char (- (skip-chars-backward "\n"))))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-remove-trailing-whitespace)
(add-hook 'emacs-everywhere-final-hooks #'emacs-everywhere-remove-trailing-whitespace)

(defun emacs-everywhere-set-frame-position ()
  "Set the size and position of the emacs-everywhere frame."
  (set-frame-position (selected-frame)
                      (- emacs-everywhere-mouse-x 100)
                      (- emacs-everywhere-mouse-y 50)))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)

(defun emacs-everywhere-insert-selection ()
  "Insert the last text selection into the buffer."
  (when-let ((selection (gui-get-selection 'PRIMARY)))
    (gui-backend-set-selection 'PRIMARY "")
    (insert selection)
    (when (and (eq major-mode 'org-mode)
               (emacs-everywhere-markdown-p))
      (shell-command-on-region (point-min) (point-max)
                               "pandoc -f markdown-auto_identifiers -t org" nil t)
      (deactivate-mark) (goto-char (point-max)))))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-insert-selection)

(when (featurep 'evil)
  (add-hook 'emacs-everywhere-init-hooks #'evil-insert-state))

(if (featurep 'spell-fu)
    (add-hook 'emacs-everywhere-init-hooks #'spell-fu-buffer)
  (when (featurep 'flyspell)
    (add-hook 'emacs-everywhere-init-hooks #'flyspell-buffer)))

(defun emacs-everywhere-markdown-p ()
  "Return t if the original window is recognised as markdown-flavoured."
  (or (cl-some (lambda (pattern)
                 (string-match-p pattern emacs-everywhere-window-title))
               emacs-everywhere-markdown-windows)
      (cl-some (lambda (pattern)
                 (string-match-p pattern emacs-everywhere-app-name))
               emacs-everywhere-markdown-apps)))

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
(add-hook 'emacs-everywhere-final-hooks #'emacs-everywhere-return-converted-org-to-gfm)

(provide 'emacs-everywhere)
;;; emacs-everywhere.el ends here
