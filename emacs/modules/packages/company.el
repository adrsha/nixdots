(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.01) ;; makes it almost immediate
  (setq company-async-wait 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-margin 1)
  (setq company-tooltip-scrollbar-width 0)
  (setq company-preview-overlay t)
  (setq company-pseudo-tooltip-overlay t)
  (setq company-global-modes '(not org-mode shell-mode))
  (setq company-backends '(company-capf))
  ;; Optional: disable icon margins
  ;; (setq company-format-margin-function nil)
  (global-company-mode 1)
  :custom-face
  (company-tooltip
   ((t (:inherit default :family cust-monospace))))
  )

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))
