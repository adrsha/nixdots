(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-on-exact-match t)     ;; Configure handling of exact matches
  (corfu-scroll-margin 0)        ;; Use scroll margin
  (corfu-minimum-width 190)        ;; Use scroll margin
  (corfu-maximum-width 190)        ;; Use scroll margin
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.3)
  (corfu-popupinfo-delay '(0.5 . 1.0))

  :config
 ;; Override frame parameters to ensure 100% opacity
  (add-to-list 'corfu--frame-parameters '(alpha . 100))
  (corfu-popupinfo-mode 1)

  (corfu-history-mode 1))
;; Add extensions
(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev 5)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )
(use-package kind-icon
  :ensure t
  :after corfu
                                        ;:custom
                                        ; (kind-icon-blend-background t)
                                        ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
