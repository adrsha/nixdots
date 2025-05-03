(use-package undo-fu
             :ensure t
             )
(use-package undo-fu-session
             :ensure t
             :config
             (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
             (undo-fu-session-global-mode))
