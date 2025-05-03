;; (advice-add 'save-buffer :around 'suppress-message-advice-around)
;; (advice-add 'kill-buffer :around 'suppress-message-advice-around)

(require 'recentf)
(add-hook 'after-save-hook 'recentf-save-list)

;; Enable display-line-numbers globally
(global-display-line-numbers-mode 1)

;; Disable display-line-numbers for specific modes
(dolist (hook '(org-mode-hook org-agenda-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))


;; Corfu
(add-hook 'eshell-mode-hook
          (lambda ()
            ;; (company-mode)
            (setq corfu-auto t)                 ;; Enable auto completion
            (setq-local corfu-auto nil)
            ))

(add-hook 'org-mode-hook
          #'(lambda ()
              (variable-pitch-mode 1)              
              (org-modern-mode 1)
              (org-indent-mode 1)
              ))


(add-hook 'org-src-mode-hook #'(lambda () (interactive) (setq header-line-format 'nil)))
(add-hook 'org-capture-mode-hook #'(lambda () (interactive) (setq header-line-format 'nil)))

