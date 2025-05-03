(add-to-list 'default-frame-alist '(internal-border-width . 40 ))
(add-to-list 'default-frame-alist '(internal-show-cursor . -1))
(add-to-list 'default-frame-alist '(alpha-background . 100))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory) ; prefix for generating autosave list file name
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
(set-display-table-slot standard-display-table 0 ?\ )

(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
