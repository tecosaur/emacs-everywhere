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
  "For use with `emacs-everywhere-major-mode-org-or-markdown'."
  :type '(rep string)
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-frame-name-format "Emacs Everywhere :: %s — %s"
  "Format string used to produce the frame name.
Formatted with the app name, and truncated window name."
  :type 'string
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-executable
  (if (eq system-type 'darwin)
      "~/.emacs-everywhere"
    (concat (or (getenv "XDG_BIN_HOME") "~/.local/bin")
            "/emacs-everywhere"))
  "Path to place or update the emacs-everywhere executable."
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

;; Semi-internal variables

(defvar emacs-everywhere-app-name nil
  "Name of the App which the original window is a instance of.")
(defvar emacs-everywhere-window-id nil
  "System ID of the original window.")
(defvar emacs-everywhere-window-name nil
  "Name (title) of the original window.")
(defvar emacs-everywhere-window-x nil
  "Leftmost pixel of the original window.")
(defvar emacs-everywhere-window-y nil
  "Top pixel of the original window.")
(defvar emacs-everywhere-window-width nil
  "Width of the original window.")
(defvar emacs-everywhere-window-height nil
  "Height of the original window.")
(defvar emacs-everywhere-mouse-x nil
  "Mouse X-coordiate at invocation.")
(defvar emacs-everywhere-mouse-y nil
  "Mouse Y-coordiate at invocation.")

;;; Executable vars

(defcustom emacs-everywhere-emacsclient-cmd
  "emacsclient --alternate-editor=\"\" --create-frame --no-wait"
  "Emacs client command to run, inserted into emacs-everywhere executable.
The emacs-everywhere call itself is appended to this string."
  :type 'string
  :group 'emacs-everywhere)

(defvar emacs-everywhere--dir (file-name-directory load-file-name))

;;; Primary functionality

;;;###autoload
(defun emacs-everywhere-initialise (app-name window-id window-name window-x window-y window-width window-height)
  "Entry point for the executable.
Provides: APP-NAME, WINDOW-ID, WINDOW-NAME, WINDOW-X, WINDOW-Y,
          WINDOW-WIDTH, WINDOW-HEIGHT."
  (switch-to-buffer (generate-new-buffer "*Emacs Everywhere*"))
  (when emacs-everywhere-major-mode-function
    ;; Only set vars that may reasonably be used,
    ;; as they are (likely) about to be cleared.
    (setq-local emacs-everywhere-app-name app-name
                emacs-everywhere-window-id window-id
                emacs-everywhere-window-name window-name)
    (funcall emacs-everywhere-major-mode-function))
  (let* ((mousepos (split-string (shell-command-to-string "xdotool getmouselocation | sed -E \"s/ screen:0 window:[^ ]*|x:|y://g\"")))
         (mouse-x (string-to-number (nth 0 mousepos)))
         (mouse-y (string-to-number (nth 1 mousepos))))
    (setq-local emacs-everywhere-app-name app-name
                emacs-everywhere-window-id window-id
                emacs-everywhere-window-name window-name
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
  (call-process "xdotool" nil nil nil
                "windowactivate" "--sync" (number-to-string emacs-everywhere-window-id))
  (when (and emacs-everywhere-paste-p (not abort))
    (if (eq system-type 'darwin)
        (call-process "osascript" nil nil nil
                      "-e" "tell application \"System Events\" to keystroke \"v\" using command down")
      (call-process "xdotool" nil nil nil
                    "key" "--clearmodifiers" "Shift+Insert")))
  (kill-buffer (current-buffer))
  (delete-frame))

;;; Setup

(defun emacs-everywhere-install ()
  "Install or update the emacs-everywhere script."
  (interactive)
  (emacs-everywhere-check-dependancies)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      (if (eq system-type 'darwin) "scripts/osx" "scripts/linux")
      emacs-everywhere--dir))
    (search-forward "{{call-emacs-client}}")
    (replace-match (concat emacs-everywhere-emacsclient-cmd
                           " --eval \"(emacs-everywhere-initialise \
$EE_APP_NAME $EE_WINDOW_ID $EE_WINDOW_TITLE \
$EE_WINDOW_X $EE_WINDOW_Y \
$EE_WINDOW_WIDTH $EE_WINDOW_HEIGHT)\""))
    (write-file emacs-everywhere-executable)
    (chmod emacs-everywhere-executable 488)
    (message "Emacs everywhere script installed to %s"
             (propertize emacs-everywhere-executable
                         'face 'font-lock-type-face))))

(defun emacs-everywhere-check-dependancies ()
  "Check that all required system executables are present."
  (let (unmet-deps)
    (dolist (dep (if (eq system-type 'darwin)
                     '("emacsclient")
                   '("emacsclient" "xclip" "xdotool" "xprop" "xwininfo")))
      (unless (executable-find dep)
        (push dep unmet-deps)))
    (when unmet-deps
      (user-error "Error. Unmet dependancies: %s" (string-join unmet-deps ", ")))))

;;; Secondary functionality

(defun emacs-everywhere-set-frame-name ()
  "Set the frame name based on `emacs-everywhere-frame-name-format'."
  (set-frame-name
   (format emacs-everywhere-frame-name-format
           emacs-everywhere-app-name
           (truncate-string-to-width
            (string-trim
             (string-trim-right emacs-everywhere-window-name
                                (format "-[A-Za-z0-9 ]*%s" emacs-everywhere-app-name))
             "[\s-]+" "[\s-]+")
            45 nil nil "…"))))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-name)

(defun emacs-everywhere-remove-trailing-whitespace ()
  "Move point to the end of the buffer, and remove all trailing whitespace."
  (goto-char (max-char))
  (delete-trailing-whitespace)
  (delete-char (- (skip-chars-backward "\n"))))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-remove-trailing-whitespace)
(add-hook 'emacs-everywhere-final-hooks #'emacs-everywhere-remove-trailing-whitespace)

(defun emacs-everywhere-set-frame-size-position ()
  "Set the size and position of the emacs-everywhere frame."
  (set-frame-size (selected-frame) 80 12)
  (set-frame-position (selected-frame)
                      (- emacs-everywhere-mouse-x 100)
                      (- emacs-everywhere-mouse-y 50)))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-size-position)

(defun emacs-everywhere-insert-selection ()
  "Insert the last text selection into the buffer."
  (when-let ((selection (gui-get-selection 'PRIMARY)))
    (gui-backend-set-selection 'PRIMARY "")
    (insert selection)
    (when (and (eq emacs-everywhere-major-mode-function #'org-mode)
               (emacs-everywhere-markdown-p))
      (shell-command-on-region (point-min) (point-max)
                               "pandoc -f markdown -t org" nil t)
      (deactivate-mark) (goto-char (point-max)))))
(add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-insert-selection)

(when (featurep 'evil)
  (add-hook 'emacs-everywhere-init-hooks #'evil-insert-state))

(when (featurep 'spell-fu)
  (add-hook 'emacs-everywhere-init-hooks #'spell-fu-buffer))

(when (featurep 'flyspell)
  (add-hook 'emacs-everywhere-init-hooks #'flyspell-buffer))

(defun emacs-everywhere-markdown-p ()
  "Return t if the original window is recognised as markdown-flavoured."
  (cl-some (lambda (pattern)
             (string-match-p pattern emacs-everywhere-window-name))
           emacs-everywhere-markdown-windows))

(defun emacs-everywhere-major-mode-org-or-markdown ()
  "Use markdow-mode, when window is recognised as markdown-flavoured.
Otherwise use `org-mode'."
  (if (emacs-everywhere-markdown-p)
      (markdown-mode)
    (org-mode)))

(defun emacs-everywhere-return-converted-org-to-gfm ()
  "When appropriate, convert org buffer to markdown."
  (when (and (eq emacs-everywhere-major-mode-function #'org-mode)
             (emacs-everywhere-markdown-p))
    (goto-char (point-min))
    (insert "#+property: header-args :exports both\n#+options: toc:nil\n")
    (let ((export-buffer (generate-new-buffer "*Emacs Everywhere Export*")))
      (org-export-to-buffer (if (featurep 'ox-gfm) 'gfm 'md) export-buffer)
      (delete-window)
      (erase-buffer)
      (insert-buffer-substring export-buffer)
      (kill-buffer export-buffer))
    (message "%s" (current-buffer))))
(add-hook 'emacs-everywhere-final-hooks #'emacs-everywhere-return-converted-org-to-gfm)

(provide 'emacs-everywhere)
;;; emacs-everywhere.el ends here
