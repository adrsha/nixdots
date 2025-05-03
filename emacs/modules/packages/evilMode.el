(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)

  ;; ----- Setting cursor colors
  (setq evil-emacs-state-cursor    '("#cba6f7" box))
  (setq evil-normal-state-cursor   '("#BAC2DE" box))
  (setq evil-operator-state-cursor '("#90b6f3" (bar . 6))) 
  (setq evil-visual-state-cursor   '("#6C7096" box))
  (setq evil-insert-state-cursor   '("#b4befe" (bar . 2)))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#f38ba8" box))
  :config
  (evil-mode 1)
  ;; INITIAL BINDINGS
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-define-key 'motion help-mode-map "q" 'kill-this-buffer)
)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :after evil)

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  :after evil)

