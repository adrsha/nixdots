(use-package iedit :ensure t)

(use-package smartparens
             :ensure t
             :config
             (sp-pair "$$" "$$")   ;; latex math mode. 

             (require 'smartparens-config)
             (add-hook 'text-mode-hook 'smartparens-mode)
             (add-hook 'prog-mode-hook 'smartparens-mode)
             (add-hook 'org-mode-hook 'smartparens-mode))

(use-package rainbow-mode
             :ensure t
             :hook (prog-mode org-mode text-mode))

(use-package rainbow-delimiters
             :ensure t
             :hook (org-mode prog-mode text-mode))

(use-package drag-stuff
             :ensure t
             :hook (org-mode . drag-stuff-mode)
             :hook (prog-mode . drag-stuff-mode))
