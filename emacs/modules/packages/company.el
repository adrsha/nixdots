(use-package company
  :ensure t
  :config
  (setq company-prefix 'nil)
  (setq company-tooltip-scrollbar-width 0)
  (setq company-tooltip-margin 1)
  (setq company-idle-delay 0.01)        ; to remove the auto complete
  (setq company-insertion-on-trigger 'nil)
  (setq company-async-wait 0.03)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations 't)
  (setq company-global-modes '(not org-mode shell-mode))
  (setq company-show-numbers 'nil)
  (setq company-preview-overlay 't)
  (setq company-pseudo-tooltip-overlay 't)
  (setq company-format-margin-function nil) ; To remove icons
  (global-company-mode 1)
  :custom-face
  (company-tooltip
   ((t (:family cust-monospace))))
  )
