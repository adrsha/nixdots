(use-package org-appear
  :ensure t
  :config
  ;; Hide org markup
  (setq-default org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'org-appear-mode)
  )
(use-package org-modern
  :ensure t
  :config
  (setq
   org-modern-star 'replace
   org-modern-replace-stars " ✽ ✣✱"
   org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
   org-modern-block-name nil
   org-modern-keyword nil
   org-modern-todo t
   org-modern-table nil
   )
  (set-face-attribute 'org-modern-done nil :foreground base :background onPrimary :weight 'bold :slant 'normal :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-todo nil :background onSecondary :foreground darkBase :weight 'bold :height 130 :inherit 'fixed-pitch)
  (set-face-attribute 'org-modern-time-inactive nil :background onSecondary :foreground darkBase :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-time-inactive nil :background onSecondary :foreground darkBase :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-time-active nil :background onSecondary :foreground darkBase :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-label nil :background darkBase :foreground onSecondary :height 130 :inherit 'nil)
)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/Course"))
  (org-roam-db-autosync-mode)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ;; ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
     ;; 	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
     ;; 	:unnarrowed t)
     ))
  :config
  (org-roam-setup))
