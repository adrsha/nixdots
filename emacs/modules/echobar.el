;;; echo-bar.el --- Turn the echo area into a customizable status bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Adarsha Acharya
;; Copyright (C) 2025  Updated Version

;; Author: Adarsha Acharya <qaiviq@gmail.com>
;; Keywords: convenience, tools
;; Version: 2.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package displays a customizable status bar in the echo area,
;; similar to desktop status bars like Polybar but inside of Emacs.
;;
;; Features:
;; - Display configurable information in the echo area
;; - Multiple built-in formatters for common information
;; - Special buffer list segment showing non-file buffers (with asterisks)
;; - Fully customizable appearance with faces
;; - Dynamic updates with configurable refresh rates
;; - Works in both regular buffer and minibuffer contexts
;;
;; Usage:
;; - Enable with M-x echo-bar-mode
;; - Customize with M-x customize-group RET echo-bar RET
;; - Create your own formatter function and set `echo-bar-function`

;;; Code:

(require 'timer)
(require 'minibuffer)
(require 'overlay)
(require 'seq)
(require 'battery)
(require 'time)
(load-file "~/.config/emacs/modules/colors.el" )

;;; Custom Faces

(defgroup echo-bar nil
  "Display text at the end of the echo area."
  :group 'applications)

;; Core faces
(defface echo-bar-default
  `((t :inherit minibuffer-prompt :foreground ,subtext0))
  "Default face for echo bar text."
  :group 'echo-bar)

(defface echo-bar-separator
  `((t :inherit echo-bar-default :foreground ,subtext0 :weight light))
  "Face for separators between sections in echo bar."
  :group 'echo-bar)

(defface echo-bar-minibuffer
  '((t :inherit echo-bar-default))
  "Face for echo bar when displayed in minibuffer."
  :group 'echo-bar)

;; Date and time faces
(defface echo-bar-date
  '((t :inherit echo-bar-default :weight regular))
  "Face for date text in echo bar."
  :group 'echo-bar)

(defface echo-bar-time
  '((t :inherit echo-bar-default :weight semibold))
  "Face for time text in echo bar."
  :group 'echo-bar)

;; Icon faces
(defface echo-bar-icon
  `((t :inherit echo-bar-default :foreground ,onSecondary))
  "Face for icons in echo bar."
  :group 'echo-bar)

(defface echo-bar-date-icon
  `((t :inherit echo-bar-icon :foreground ,onSecondary))
  "Face for date icon in echo bar."
  :group 'echo-bar)

(defface echo-bar-time-icon
  `((t :inherit echo-bar-icon :foreground ,onSecondary))
  "Face for time icon in echo bar."
  :group 'echo-bar)

;; Battery faces
(defface echo-bar-battery-icon
  '((t :inherit echo-bar-icon))
  "Face for battery icons in echo bar."
  :group 'echo-bar)

(defface echo-bar-battery-charging
  `((t :inherit echo-bar-default :foreground ,onSecondary :weight semibold))
  "Face for charging battery indicator in echo bar."
  :group 'echo-bar)

(defface echo-bar-battery-high
  `((t :inherit echo-bar-default :foreground ,onSecondary :weight semibold))
  "Face for high battery level indicator in echo bar."
  :group 'echo-bar)

(defface echo-bar-battery-medium
  `((t :inherit echo-bar-default :foreground ,yellow :weight semibold))
  "Face for medium battery level indicator in echo bar."
  :group 'echo-bar)

(defface echo-bar-battery-low
  `((t :inherit echo-bar-default :foreground ,red :weight bold))
  "Face for low battery level indicator in echo bar."
  :group 'echo-bar)

;; Information faces
(defface echo-bar-info
  '((t :inherit echo-bar-default :weight light))
  "Face for general information display in echo bar."
  :group 'echo-bar)

(defface echo-bar-info-highlighted
  '((t :inherit echo-bar-info :weight semibold))
  "Face for highlighted information in echo bar."
  :group 'echo-bar)

(defface echo-bar-buffer-name
  '((t :inherit echo-bar-default :weight semibold))
  "Face for buffer name in echo bar."
  :group 'echo-bar)

(defface echo-bar--directory
  `((t :inherit echo-bar-default :weight semibold :foreground ,onPrimary))
  "Face for buffer name in echo bar."
  :group 'echo-bar)

(defface echo-bar-major-mode
  '((t :inherit echo-bar-default :slant italic))
  "Face for major mode name in echo bar."
  :group 'echo-bar)

(defface echo-bar-position
  '((t :inherit echo-bar-default ))
  "Face for cursor position info in echo bar."
  :group 'echo-bar)

(defface echo-bar-line-count
  '((t :inherit echo-bar-default ))
  "Face for line count in echo bar."
  :group 'echo-bar)

;; Special state faces
(defface echo-bar-modified
  `((t :inherit echo-bar-default :foreground ,peach :weight bold))
  "Face for modified buffer indicator in echo bar."
  :group 'echo-bar)

(defface echo-bar-read-only
  `((t :inherit echo-bar-default :foreground ,subtext0 :slant italic))
  "Face for read-only buffer indicator in echo bar."
  :group 'echo-bar)

(defface echo-bar-non-special-buffers
  '((t :inherit echo-bar-default))
  "Face for non-special buffer names in the echo bar."
  :group 'echo-bar-faces)

(defface echo-bar-buffer-count
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for buffer counts in the echo bar."
  :group 'echo-bar-faces)

;;; Customization Options

(defcustom echo-bar-function #'echo-bar-default-function
  "Function that returns the text displayed in the echo bar.
This function should return a string or a list of strings and
propertized text to be displayed in the echo bar."
  :group 'echo-bar
  :type 'function)

(defcustom echo-bar-format
  '(:eval 
    (list
     (when echo-bar-show-buffer-info
       (list
        (echo-bar-segment-buffer-name)
        (echo-bar-separator)))
     (when echo-bar-show-non-special-buffers
       (list
        (echo-bar-segment-non-special-buffers)
        (echo-bar-separator)))
     (when echo-bar-show-position
       (list
        (echo-bar-segment-position)
        (echo-bar-separator)))
     (when echo-bar-show-line-count
       (list
        (echo-bar-segment-line-count)
        (echo-bar-separator)))
     (when echo-bar-show-date
       (echo-bar-segment-date))
     (when echo-bar-show-time
       (list
        (echo-bar-separator)
        (echo-bar-segment-time)))
     (when (and echo-bar-show-battery 
                battery-status-function 
                (not (string= (getenv "INSIDE_EMACS") "vterm")))
       (list
        (echo-bar-separator)
        (echo-bar-segment-battery)))))
  "Format of the text displayed in the echo bar.

This format will only apply if `echo-bar-function' is set to
`echo-bar-default-function', otherwise, the output of
`echo-bar-function' will be used.

Can be a string, a mode-line format list, or a function that returns
one of those. See `mode-line-format' for more info about the required format."
  :group 'echo-bar
  :type 'sexp)

;; Segment toggles
(defcustom echo-bar-show-buffer-info t
  "If non-nil, show buffer name in the echo bar."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-show-non-special-buffers t
  "Weather to let non special buffers to display in the echo bar."
  :group 'echo-bar
  :type 'integer)

;; Suggested customization variables to add:
(defcustom echo-bar-non-special-buffers-max 3
  "Maximum number of non-special buffers to display in the echo bar."
  :type 'integer
  :group 'echo-bar)

(defcustom echo-bar-non-special-buffers-max-name-length 20
  "Maximum length of non-special buffer names in the echo bar."
  :type 'integer
  :group 'echo-bar)

(defcustom echo-bar-show-position t
  "If non-nil, show cursor position in the echo bar."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-show-line-count t
  "If non-nil, show line count in the echo bar."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-show-date t
  "If non-nil, show date in the echo bar."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-show-time t
  "If non-nil, show time in the echo bar."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-show-battery t
  "If non-nil, show battery status in the echo bar when available."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-separator "   "
  "Separator string used between segments in the default format."
  :group 'echo-bar
  :type 'string)

(defcustom echo-bar-right-padding 4
  "Number of columns between the text and right margin."
  :group 'echo-bar
  :type 'number)

(defcustom echo-bar-minibuffer t
  "If non-nil, also display the echo bar when in the minibuffer."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-update-interval 0.2
  "Interval in seconds between updating time-based elements in the echo bar.
This affects date, time, and battery status updates.
Other elements update immediately when buffers or window configurations change.
Set to nil to disable automatic updates of time-based elements."
  :group 'echo-bar
  :type '(choice (const :tag "No automatic updates for time elements" nil)
                 (number :tag "Seconds")))

(defcustom echo-bar-date-format "%b %d"
  "Format string for the date segment.
See `format-time-string' for details."
  :group 'echo-bar
  :type 'string)

(defcustom echo-bar-time-format "%H:%M:%S"
  "Format string for the time segment.
See `format-time-string' for details."
  :group 'echo-bar
  :type 'string)

(defcustom echo-bar-use-icons t
  "Whether to use icons in the echo bar.
If non-nil and a suitable font is available, use icons in the display.
If nil, use text alternatives."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-battery-low-threshold 20
  "Battery percentage at which the battery indicator turns red."
  :group 'echo-bar
  :type 'integer)

(defcustom echo-bar-battery-medium-threshold 50
  "Battery percentage at which the battery indicator turns yellow."
  :group 'echo-bar
  :type 'integer)

(defcustom echo-bar-prevent-message-overwrite nil
  "If non-nil, temporarily hide the echo bar when messages are displayed.
This prevents the echo bar from competing with important messages."
  :group 'echo-bar
  :type 'boolean)

(defcustom echo-bar-position 'right
  "Position of the echo bar in the echo area.
Can be 'left, 'right, or 'center."
  :group 'echo-bar
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Center" center)))

(defcustom echo-bar-height 140
  "Height of the echo bar text as a percentage of the default face height."
  :group 'echo-bar
  :type 'integer)

;;; Internal Variables

(defvar echo-bar-timer nil
  "Timer used to update the echo bar.")

(defvar echo-bar-text nil
  "The text currently displayed in the echo bar.")

(defvar echo-bar-overlays nil
  "List of overlays displaying the echo bar contents.")

(defvar echo-bar-last-update-time 0
  "Time of the last echo bar update in seconds.")

(defvar echo-bar-suppress nil
  "When non-nil, temporarily suppress echo bar display.")

;;; Core Functions

;;;###autoload
(define-minor-mode echo-bar-mode
  "Display text at the end of the echo area."
  :global t
  (if echo-bar-mode
      (echo-bar-enable)
    (echo-bar-disable)))

;;;###autoload
(defun echo-bar-configure-faces ()
  "Configure faces for echo-bar based on current settings."
  (let ((height echo-bar-height))
    ;; Core faces
    (set-face-attribute 'echo-bar-default nil :height height)
    (set-face-attribute 'echo-bar-icon nil :height (+ height 10))
    
    ;; Ensure the minibuffer face has proper height
    (set-face-attribute 'echo-bar-minibuffer nil :height (+ height 10))))

;; Add hooks to update the echo bar on buffer switching events
(defun echo-bar-enable ()
  "Turn on the echo bar."
  (interactive)
  ;; Disable any existing echo bar to remove conflicts
  (echo-bar-disable)

  ;; Configure faces first
  (echo-bar-configure-faces)

  ;; Create overlays in each echo area buffer
  (dolist (buf (mapcar #'get-buffer-create
                       '(" *Echo Area 0*" " *Echo Area 1*")))
    (with-current-buffer buf
      (remove-overlays (point-min) (point-max) 'echo-bar t)
      (echo-bar--new-overlay)))

  ;; Start the timer to automatically update time-related information
  ;; We'll keep this for time/date/battery updates but at a much lower frequency
  (when echo-bar-update-interval
    (setq echo-bar-timer 
          (run-with-timer 0 (or echo-bar-update-interval 1.0) 'echo-bar-update-time-segments)))
  
  ;; Add buffer switching hooks for event-based updates
  (add-hook 'window-buffer-change-functions #'echo-bar-buffer-change)
  (add-hook 'buffer-list-update-hook #'echo-bar-buffer-change)
  (add-hook 'focus-in-hook #'echo-bar-buffer-change)
  
  ;; Update immediately
  (echo-bar-update)

  ;; Add minibuffer hook
  (when echo-bar-minibuffer
    (add-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup))
  
  ;; Add advice to message function if we want to prevent overwrite
  (when echo-bar-prevent-message-overwrite
    (advice-add 'message :around #'echo-bar--message-advice)))

(defun echo-bar-disable ()
  "Turn off the echo bar."
  (interactive)
  ;; Remove echo bar overlays
  (mapc 'delete-overlay echo-bar-overlays)
  (setq echo-bar-overlays nil)

  ;; Remove text from Minibuf-0
  (with-current-buffer (window-buffer
                        (minibuffer-window))
    (delete-region (point-min) (point-max)))

  ;; Cancel the update timer
  (when echo-bar-timer
    (cancel-timer echo-bar-timer)
    (setq echo-bar-timer nil))

  ;; Remove hooks and advice
  (remove-hook 'minibuffer-setup-hook #'echo-bar--minibuffer-setup)
  (remove-hook 'window-buffer-change-functions #'echo-bar-buffer-change)
  (remove-hook 'buffer-list-update-hook #'echo-bar-buffer-change)
  (remove-hook 'focus-in-hook #'echo-bar-buffer-change)
  (advice-remove 'message #'echo-bar--message-advice))

;; Event handler for buffer changes
(defun echo-bar-buffer-change (&rest _)
  "Update echo bar in response to buffer changes."
  (when echo-bar-mode
    ;; Throttle updates to prevent excessive updates during rapid buffer changes
    (let ((current-time (float-time)))
      (when (> (- current-time echo-bar-last-update-time) 0.1)
        (setq echo-bar-last-update-time current-time)
        (echo-bar-update)))))

;; Separate update function for time-based elements
(defun echo-bar-update-time-segments ()
  "Update the time-based segments of the echo bar (time, date, battery)."
  (when (and echo-bar-mode
             (or (not echo-bar-prevent-message-overwrite)
                 (not (current-message))))
    (let ((current-time (float-time)))
      (setq echo-bar-last-update-time current-time)
      (echo-bar-update))))

(defun echo-bar--string-pixel-width (str)
  "Return the width of STR in pixels."
  ;; Make sure the temp buffer settings match the minibuffer settings
  (with-selected-window (minibuffer-window)
    (if (fboundp #'string-pixel-width)
        (string-pixel-width str)
      (require 'shr)
      (shr-string-pixel-width str))))

(defun echo-bar--str-len (str)
  "Calculate STR length in character columns based on pixel width."
  (let ((width (frame-char-width))
        (len (echo-bar--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeds

(defun echo-bar-set-text (text)
  "Set the text displayed by the echo bar to TEXT."
  (when text
    (let* ((text-str (if (stringp text) text (format-mode-line text)))
           (wid (+ (echo-bar--str-len text-str) echo-bar-right-padding))
           ;; Maximum length for the echo area message before wrap to next line
           (max-len (- (frame-width) wid 5))
           ;; Align the text according to chosen position
           (spc (propertize " " 'cursor 1 'display
                            (pcase echo-bar-position
                              ('right `(space :align-to (- right-fringe ,wid)))
                              ('center `(space :align-to (- center ,(/ wid 2))))
                              (_ `(space :align-to 0))))))

      (setq echo-bar-text (concat spc text-str))

      ;; Add the correct text to each echo bar overlay
      (dolist (o echo-bar-overlays)
        (when (overlay-buffer o)
          (with-current-buffer (overlay-buffer o)
            (let ((line-too-long (> (mod (point-max) (frame-width)) max-len)))
              (overlay-put o 'after-string 
                           (if line-too-long
                               (concat "\n" echo-bar-text)
                             echo-bar-text))))))

      ;; Display the text in Minibuf-0, as overlays don't show up there
      (with-current-buffer (window-buffer
                            (minibuffer-window))
        ;; Don't override existing text in minibuffer
        (when (get-text-property (point-min) 'echo-bar)
          (delete-region (point-min) (point-max)))
        (when (and (= (point-min) (point-max))
                   (not echo-bar-suppress))
          (insert (propertize echo-bar-text 'echo-bar t)))))))

(defun echo-bar--new-overlay (&optional remove-dead buffer)
  "Add new echo-bar overlay to BUFFER.
When REMOVE-DEAD is non-nil, also remove any dead overlays, i.e.,
those without a buffer from the beginning of the internal list of
overlays."
  (when remove-dead
    ;; Remove all dead overlays from the list
    (setq echo-bar-overlays
          (seq-filter 'overlay-buffer echo-bar-overlays)))

  (let ((new-overlay (make-overlay (point-max)
                                   (point-max) buffer t t)))
    (overlay-put new-overlay 'echo-bar t)
    (push new-overlay echo-bar-overlays)
    new-overlay))

(defun echo-bar--minibuffer-setup ()
  "Setup the echo bar in the minibuffer."
  (overlay-put (echo-bar--new-overlay t) 'priority 1)
  (echo-bar-update))

(defun echo-bar--message-advice (orig-fun &rest args)
  "Advice function for `message' to handle echo bar display.
Temporarily hides the echo bar when a message is displayed.
ORIG-FUN is the original function and ARGS are its arguments."
  (let ((echo-bar-suppress t))
    (apply orig-fun args)
    ;; Schedule a refresh after the message is displayed
    (when echo-bar-mode
      (run-with-timer 2 nil #'echo-bar-update))))


;; main update function 
(defun echo-bar-update ()
  "Update the text displayed in the echo bar.
Gets new text from `echo-bar-function'."
  (interactive)
  (when (and echo-bar-mode
             (or (not echo-bar-prevent-message-overwrite)
                 (not (current-message))))
    (echo-bar-set-text (funcall echo-bar-function))))

;;; Helper Functions


(defun echo-bar--buffer-special-p (buffer)
  "Return non-nil if BUFFER is a special buffer.
Special buffers include:
- Buffers whose names start and end with *
- Buffers whose names start with spaces
- Buffers whose names start with .
- Buffers that don't have associated files
- Internal buffers for magit, dired, etc."
  (let ((name (buffer-name buffer)))
    (or
     ;; Standard pattern for special buffers: *name*
     (and name (string-match-p "^\\*.*\\*$" name))
     ;; Buffers starting with space
     (and name (string-match-p "^ " name))
     ;; Hidden buffers starting with dot
     (and name (string-match-p "^\\." name))
     ;; Mode-specific special buffers
     (with-current-buffer buffer
       (or
        ;; Non-file buffers (unless they've been explicitly saved)
        (and (not buffer-file-name)
             ;; Exclude buffers that might be non-file but still "regular"
             (not (derived-mode-p 'text-mode 'prog-mode 'conf-mode)))
        ;; Special modes that don't necessarily follow naming conventions
        (derived-mode-p 'dired-mode 'magit-mode 'help-mode 'Info-mode
                       'compilation-mode 'completion-list-mode))))))

(defun echo-bar--get-special-buffers ()
  "Return a list of special buffers (with * at beginning and end)."
  (seq-filter #'echo-bar--buffer-special-p (buffer-list)))

(defun echo-bar--truncate-buffer-name (name max-length)
  "Truncate buffer NAME to MAX-LENGTH characters with ellipsis if needed."
  (if (<= (length name) max-length)
      name
    (concat (substring name 0 (- max-length 1)) "…")))

;;;; Segment Functions

(defun echo-bar-separator ()
  "Return the separator string with proper face."
  (propertize echo-bar-separator 'face 'echo-bar-separator))

(defun echo-bar-segment-date ()
  "Return formatted date for the echo bar."
  (let ((date (format-time-string echo-bar-date-format)))
    (if echo-bar-use-icons
        (concat 
         (propertize "󰃭 " 'face 'echo-bar-date-icon)
         (propertize date 'face 'echo-bar-date))
      (propertize date 'face 'echo-bar-date))))

(defun echo-bar-segment-time ()
  "Return formatted time for the echo bar."
  (let ((time (format-time-string echo-bar-time-format)))
    (if echo-bar-use-icons
        (concat 
         (propertize "󰥔 " 'face 'echo-bar-time-icon)
         (propertize time 'face 'echo-bar-time))
      (propertize time 'face 'echo-bar-time))))

(defun echo-bar-segment-battery ()
  "Return battery information for the echo bar."
  (when battery-status-function
    (let* ((status (funcall battery-status-function))
           (percent (if status 
                        (round (string-to-number (battery-format "%p" status)))
                      0))
           (power-method (if status (battery-format "%L" status) ""))
           (charging (string= power-method "AC"))
           (face (cond 
                  (charging 'echo-bar-battery-charging)
                  ((< percent echo-bar-battery-low-threshold) 'echo-bar-battery-low)
                  ((< percent echo-bar-battery-medium-threshold) 'echo-bar-battery-medium)
                  (t 'echo-bar-battery-high)))
           (icon (cond
                  (charging " ")
                  ((< percent 10) " ")
                  ((< percent 20) " ")
                  ((< percent 30) " ")
                  ((< percent 40) " ")
                  ((< percent 50) " ")
                  ((< percent 60) " ")
                  ((< percent 70) " ")
                  ((< percent 80) " ")
                  ((< percent 90) " ")
                  (t " "))))
      (if echo-bar-use-icons
          (concat
           (propertize icon 'face 'echo-bar-battery-icon)
           (when charging
             (propertize "  " 'face 'echo-bar-battery-charging))
           (propertize (format "%d%%" percent) 'face face))
        (concat
         (propertize "BAT:" 'face 'echo-bar-battery-icon)
         (when charging
           (propertize "[+] " 'face 'echo-bar-battery-charging))
         (propertize (format "%d%%" percent) 'face face))))))

(defun echo-bar-segment-line-count ()
  "Return buffer line count for the echo bar."
  (let ((count (count-lines (point-min) (point-max))))
    (if echo-bar-use-icons
        (concat
         (propertize "󰅩 " 'face 'echo-bar-icon)
         (propertize (format "%d" count) 'face 'echo-bar-line-count))
      (propertize (format "%d lines" count) 'face 'echo-bar-line-count))))

(defun echo-bar-segment-buffer-name ()
  "Return current buffer name with parent directory for the echo bar."
  (let* ((name (buffer-name))
         (modified (buffer-modified-p))
         (read-only buffer-read-only)
         (file-name (or (buffer-file-name) ""))
         (directory (when (and file-name (not (string-empty-p file-name)))
                      (file-name-directory file-name))))
    (concat
     (if echo-bar-use-icons
         (propertize "󰈙 " 'face 'echo-bar-icon)
       "")
     (when directory
       (propertize 
        (file-name-nondirectory 
         (directory-file-name 
          (file-name-directory directory)))
        'face 'echo-bar-directory))
     (when directory
       (propertize "/" 'face 'echo-bar-directory))
     (propertize name 'face 'echo-bar-buffer-name)
     (cond
      (read-only (propertize " [RO]" 'face 'echo-bar-read-only))
      (modified (propertize " [+]" 'face 'echo-bar-modified))
      (t "")))))


(defun echo-bar-segment-non-special-buffers ()
  "Return formatted string of non-special buffers for the echo bar.
Non-special buffers are those that aren't identified by `echo-bar--buffer-special-p'.
The current buffer is excluded from the displayed list."
  (let* ((current-buf (current-buffer))
         (non-special-buffers
          (seq-filter
           (lambda (buf)
             (and (not (echo-bar--buffer-special-p buf))
                  (not (eq buf current-buf))))
           (buffer-list)))
         (count (length non-special-buffers))
         (display-count (min count echo-bar-non-special-buffers-max))
         (hidden-count (- count display-count)))
    
    (if (zerop count)
        (propertize "No regular buffers" 'face 'echo-bar-non-special-buffers)
      (concat
       (if echo-bar-use-icons
           (propertize " " 'face 'echo-bar-icon)
         "")
       (propertize 
        (format "%d" count)
        'face 'echo-bar-buffer-count)
       ": "
       (mapconcat 
        (lambda (buf)
          (let* ((name (echo-bar--truncate-buffer-name 
                        (buffer-name buf) 
                        echo-bar-non-special-buffers-max-name-length))
                 (face 'echo-bar-non-special-buffers))
            (propertize name 'face face)))
        (seq-take non-special-buffers display-count)
        ", ")
       (when (> hidden-count 0)
         (propertize (format " +%d more" hidden-count) 
                     'face 'echo-bar-info))))))


(defun echo-bar-segment-major-mode ()
  "Return major mode name for the echo bar."
  (let ((mode-name (format-mode-line mode-name)))
    (if echo-bar-use-icons
        (concat
         (propertize "󰘧 " 'face 'echo-bar-icon)
         (propertize mode-name 'face 'echo-bar-major-mode))
      (propertize mode-name 'face 'echo-bar-major-mode))))

(defun echo-bar-segment-position ()
  "Return cursor position info for the echo bar."
  (let ((line (format-mode-line "%l"))
        (column (format-mode-line "%c"))
        (percent (format-mode-line "%p")))
    (if echo-bar-use-icons
        (concat
         (propertize "󰯑 " 'face 'echo-bar-icon)
         (propertize (format "%s:%s" line column) 'face 'echo-bar-position))
      (propertize (format "%s:%s" line column) 'face 'echo-bar-position))))

(defun echo-bar-segment-project ()
  "Return project name for the echo bar if available."
  (when (fboundp 'project-current)
    (when-let ((project (project-current nil))
               (name (project-name project)))
      (if echo-bar-use-icons
          (concat
           (propertize "󰏓 " 'face 'echo-bar-icon)
           (propertize name 'face 'echo-bar-info-highlighted))
        (propertize name 'face 'echo-bar-info-highlighted)))))

;;;; Default Function

(defun echo-bar-default-function ()
  "The default function to use for the contents of the echo bar.
Returns the formatted text from `echo-bar-format'."
  (cond
   ((functionp echo-bar-format)
    (funcall echo-bar-format))
   (t (format-mode-line echo-bar-format))))

(provide 'echo-bar)
;;; echo-bar.el ends here
