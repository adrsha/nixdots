(use-package nerd-icons
             :ensure t
             :config
             :if (display-graphic-p))

(use-package nerd-icons-completion
             :ensure t
             :config
             (nerd-icons-completion-mode)
             )

(use-package nerd-icons-dired
             :ensure t
             :hook
             (dired-mode . nerd-icons-dired-mode))
