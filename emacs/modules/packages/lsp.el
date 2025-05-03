
(use-package lsp-mode
  :ensure t
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :hook
  (
   ;; only enable lsp for prog-mode if it's not emacs-lisp-mode
   (prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode)
                    (lsp-deferred))))
   )

  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-lens-enable 't)
  (setq lsp-idle-delay 0.0)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-mode nil)
  (setq lsp-eldoc-enable-hover nil)     ; Eldoc
  (setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
  (setq lsp-signature-render-documentation t)
  (setq lsp-completion-provider :none) ;; we use Corfu!
  (setq lsp-completion-show-detail t)
  (setq lsp-use-plists t)

  (setq lsp-ui-doc-frame-parameters
        '((left . -1)
          (no-focus-on-map . t)
          (alpha-background . 100)
          (min-width  . 0)
          (width  . 0)
          (min-height  . 0)
          (height  . 0)
          (internal-border-width . 15)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (right-fringe . 0)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (tab-bar-lines . 0)
          (tab-bar-lines-keep-state . 0)
          (line-spacing . 0)
          (unsplittable . t)
          (undecorated . t)
          (bottom . -1)
          (visibility . nil)
          (mouse-wheel-frame . nil)
          (no-other-frame . t)
          (inhibit-double-buffering . t)
          (drag-internal-border . t)
          (no-special-glyphs . t)
          (desktop-dont-save . t)))


  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-border darkBase)

  (set-face-attribute 'lsp-ui-doc-background nil :background darkBase :foreground darkBase)

  ;; Override lsp-ui-doc--handle-hr-lines
  (defun lsp-ui-doc--handle-hr-lines ()
    (let (bolp next before after)
      (goto-char 1)
      (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
        (when (get-text-property next 'markdown-hr)
          (goto-char next)
          (setq bolp (bolp)
                before (char-before))
          (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
          (setq after (char-after (1+ (point))))
          (insert
           (concat
            (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
            (propertize " "
                        'display '(space :height (1))
                        'lsp-ui-doc--replace-hr t
                        'face '(:background base))
            (propertize " " 'display '(space :height (1)))
            (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.2))))))))))

(use-package markdown-mode
  :ensure t)

(use-package prettier
  :ensure t)
