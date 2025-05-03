(setq dired-use-ls-dired nil)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq-default dired-kill-when-opening-new-dired-buffer 't)
(setq dired-listing-switches "-Agho --group-directories-first")

(defun config-dired ()
  "Dired hook."
  (evil-collection-define-key 'normal 'dired-mode-map
    "l" 'dired-find-alternate-file
    "h" 'dired-up-directory
    "n" 'dired-create-empty-file
    "y" 'dired-do-copy
    "d" 'dired-do-delete
    "r" 'dired-do-rename
    "u" 'dired-undo
    "=" 'dired-diff
    "X" 'dired-do-chmod
    "m" 'dired-toggle-marks
    "q" 'kill-buffer-and-window
    )
  (set-face-attribute 'dired-header nil :foreground onPrimary :weight 'bold :font cust-serif :height 190)
  (face-remap-add-relative 'default '(:family "Inter Variable" :height 140 :weight 'semibold)))

(add-hook 'dired-mode-hook 'config-dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
