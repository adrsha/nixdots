;; Enable vertico
(use-package vertico
             :ensure t
             :init
             (vertico-mode)
             (vertico-reverse-mode)
             ;; Different scroll margin
             (setq vertico-scroll-margin 2)

             ;; Show more candidates
             (setq vertico-count 5)

             ;; Grow and shrink the Vertico minibuffer
             (setq vertico-resize t)

             ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
             (setq vertico-cycle t)

             (add-hook 'minibuffer-mode-hook (lambda () (interactive)
                                               (setq-local face-remapping-alist '((default minibuffer-face))))))

(use-package consult
             :ensure t
             :init
             (setq register-preview-delay 0.5
                   ;; register-preview-function #'consult-register-format)
                   register-preview-function 'nil)

             ;; (advice-add #'register-preview :override #'consult-register-window)
             (setq xref-show-xrefs-function #'consult-xref
                   xref-show-definitions-function #'consult-xref)
             :config
             (add-to-list 'consult-buffer-filter "\*.*\*")
             ;; Allowing single key press to begin asynchorous searches like consult-grep
             (setq consult-async-min-input 1)

             (consult-customize
               consult-theme consult-buffer :preview-key 'nil
               consult-recent-file :preview-key "C-h"
               consult-ripgrep consult-git-grep consult-grep
               consult-bookmark consult-xref
               consult--source-bookmark consult--source-file-register
               consult--source-recent-file consult--source-project-recent-file
               ;; :preview-key "M-."
               :preview-key '(:debounce 0.4 any))

             (setq consult-narrow-key "<") ;; "C-+"
             )

(use-package embark :ensure t)
(use-package embark-consult :ensure t)
