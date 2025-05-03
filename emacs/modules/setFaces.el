(defun configure-font ()
  "Configure font given initial non-daemon FRAME.
 Intended for `after-make-frame-functions'."
 (set-face-attribute 'default nil :font cust-monospace :height 130 :weight 'normal)
 (set-face-attribute 'fixed-pitch nil :font cust-monospace :height 130)
 (set-face-attribute 'variable-pitch nil :font cust-sans-serif :height 150)
 (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
 (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
 (set-face-attribute 'line-number nil :font cust-monospace :height 110)
 (set-face-attribute 'link nil :background surface0 :slant 'oblique  :weight 'regular :overline nil :underline nil :family cust-serif)
 (set-face-attribute 'show-paren-match nil :foreground text 
                     :background 'unspecified :underline nil)
 (set-face-attribute 'show-paren-match-expression nil :background surface1 :foreground 'unspecified :inherit nil)
 (set-face-attribute 'help-key-binding nil :font cust-sans-serif 
                     :weight 'semibold :background surface0 
                     :foreground text :box nil)
 (set-face-attribute 'header-line nil :background base :foreground text)
 (set-face-attribute 'window-divider nil :background base :foreground base)
 (set-face-attribute 'region nil :background surface1)
 )

;; Use after-init-hook for non-daemon mode
(add-hook 'after-init-hook
          (lambda ()
            (when (display-graphic-p)
              (configure-font))))

;; Use after-make-frame-functions for daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (when (display-graphic-p)
                (configure-font)))))

(defun configure-vertico-font ()
  "Configure font given initial non-daemon FRAME.
 Intended for `after-make-frame-functions'."
  (interactive)
  (set-face-attribute 'vertico-current nil :foreground onPrimary :weight 'semibold :background darkBase)
  (set-face-attribute 'vertico-multiline nil :weight 'semibold :height 170)
  (set-face-attribute 'minibuffer-prompt nil :foreground onSecondary :weight 'semibold :background base :height 140)
  ;; (set-face-attribute 'minibuffer-face nil :height 170 )
  )
(add-hook 'server-after-make-frame-hook #'configure-vertico-font)

(defun configure-parens-font ()
  "Configure font given initial non-daemon FRAME.
   Intended for `after-make-frame-functions'."
  (set-face-attribute 'sp-show-pair-enclosing nil :background lavender :underline t)
  (set-face-attribute 'sp-pair-overlay-face nil :background lavender :underline t)
  )
(add-hook 'smartparens-mode-hook #'configure-parens-font)

(defun configure-evil-font ()
  "Configure font given initial non-daemon FRAME.
 Intended for `after-make-frame-functions'."
  (set-face-attribute 'evil-ex-info nil :foreground red :slant 'oblique )
  (set-face-attribute 'evil-ex-substitute-matches nil :background blue :foreground darkBase :strike-through 't :underline 'nil )
  (set-face-attribute 'evil-ex-substitute-replacement nil :background teal :foreground darkBase :underline 'nil )
  )

(add-hook 'server-after-make-frame-hook 'configure-evil-font)

(defun configure-flymake-font ()
  "Configure font given initial non-daemon FRAME.
   Intended for `lsp-mode'."
  (set-face-attribute 'flymake-error nil :background "#42232c" :foreground red :underline 'nil :weight 'bold)
  (set-face-attribute 'flymake-note nil :background "#262d25" :foreground green :underline 'nil :weight 'bold)
  (set-face-attribute 'flymake-warning nil :background "#453e29" :foreground yellow :underline 'nil :weight 'bold)
  )
(add-hook 'flymake-mode-hook #'configure-flymake-font)

(defun configure-lsp-font ()
  "Configure font given initial non-daemon FRAME.
   Intended for `after-make-frame-functions'."
  (set-face-attribute 'lsp-face-highlight-textual nil :foreground 'unspecified :background surface1 :inherit 'nil :weight 'bold :italic t)
  (set-face-attribute 'lsp-face-highlight-write nil :foreground 'unspecified :background surface1 :inherit 'nil :weight 'bold)
  (set-face-attribute 'lsp-face-highlight-read nil :underline 'nil :foreground 'unspecified :background surface1 :inherit 'nil :weight 'bold )
  )
(add-hook 'lsp-mode-hook 'configure-lsp-font)

(defun configure-lspui-font ()
  "Configure font for LSP UI."
  (when (facep 'lsp-headerline-breadcrumb-symbols-error-face)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-error-face nil :underline nil :background "#42232c" :foreground red)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-warning-face nil :underline nil :background "#453e29" :foreground yellow)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-info-face nil :underline nil :background "#262d25" :foreground green)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-error-face nil :underline nil :background "#42232c" :foreground red)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-warning-face nil :underline nil :background "#453e29" :foreground yellow)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-info-face nil :underline nil :background "#262d25" :foreground green)))

(with-eval-after-load 'lsp-mode
  (configure-lspui-font))

(defun configure-org-font ()
  "Configure font given initial non-daemon FRAME.
 Intended for `after-make-frame-functions'."
  (set-face-attribute 'org-block nil :background darkBase :font cust-monospace)
  (set-face-attribute 'org-verbatim nil :background 'unspecified :foreground subtext0 :inherit 'fixed-pitch :weight 'semibold :height 150)
  (set-face-attribute 'org-block-end-line nil :background darkBase :foreground darkBase :font cust-sans-serif :height 200)
  (set-face-attribute 'org-block-begin-line nil :background darkBase :foreground darkBase :font cust-sans-serif :height 200)
  (set-face-attribute 'org-meta-line nil :slant 'normal :height 90 :foreground base :font cust-serif)
  (set-face-attribute 'org-drawer nil :foreground base)
  (set-face-attribute 'org-todo nil :background base :foreground onSecondary :weight 'bold :font cust-sans-serif :height 200)
  (set-face-attribute 'org-agenda-diary nil :foreground blue :weight 'bold :font cust-sans-serif)
  (set-face-attribute 'org-property-value nil :foreground base :weight 'bold :font cust-sans-serif)
  (set-face-attribute 'org-special-keyword nil :foreground base :weight 'bold :font cust-sans-serif)

  (set-face-attribute 'org-document-info-keyword nil :foreground base)
  ;; (set-face-attribute 'org-level-1 nil :height 235 :family cust-serif :weight 'semibold )
  ;; (set-face-attribute 'org-level-2 nil :height 220 :family cust-serif :weight 'semibold )
  ;; (set-face-attribute 'org-level-3 nil :height 205 :family cust-serif :weight 'regular )
  ;; (set-face-attribute 'org-level-4 nil :height 190 :family cust-serif :weight 'regular )
  ;; (set-face-attribute 'org-level-5 nil :height 190 :family cust-serif :weight 'regular )
  ;; (set-face-attribute 'org-level-6 nil :height 190 :family cust-serif :weight 'regular )
  ;; (set-face-attribute 'org-level-7 nil :height 190 :family cust-serif :weight 'regular )
  ;; (set-face-attribute 'org-level-8 nil :height 190 :family cust-serif :weight 'regular )
  (set-face-attribute 'org-level-1 nil :height 270 :family cust-serif :foreground onSecondary :weight 'semibold )
  (set-face-attribute 'org-level-2 nil :height 200 :family cust-serif :foreground onSecondary :weight 'semibold )
  (set-face-attribute 'org-level-3 nil :height 200 :family cust-serif :foreground onSecondary :weight 'regular )
  (set-face-attribute 'org-level-4 nil :height 190 :family cust-serif :foreground onSecondary :weight 'regular )
  (set-face-attribute 'org-level-5 nil :height 180 :family cust-serif :foreground onSecondary :weight 'regular )
  (set-face-attribute 'org-level-6 nil :height 170 :family cust-serif :foreground onSecondary :weight 'regular )
  (set-face-attribute 'org-level-7 nil :height 170 :family cust-serif :foreground onSecondary :weight 'regular )
  (set-face-attribute 'org-level-8 nil :height 170 :family cust-serif :foreground onSecondary :weight 'regular )
  (set-face-attribute 'org-table nil :background darkBase :inherit 'fixed-pitch)

  (set-face-attribute 'org-document-title nil :height 280 :font cust-serif :foreground onPrimary :weight 'semibold)
  (set-face-attribute 'org-ellipsis nil :slant 'normal :foreground subtext1)
  (set-face-attribute 'org-done nil :slant 'normal :strike-through 'nil :foreground subtext1)

  (set-face-attribute 'org-agenda-date nil :font cust-sans-serif :weight 'regular :height 200 :foreground subtext1)
  (set-face-attribute 'org-agenda-date-today nil :font cust-sans-serif :weight 'semibold :height 200 )
  (set-face-attribute 'org-agenda-done nil :font cust-serif :weight 'regular :height 190 :strike-through 't)
  (set-face-attribute 'org-agenda-structure nil :font cust-serif :weight 'regular :height 230 :foreground lavender)
  )

(add-hook 'org-mode-hook #'configure-org-font)

(defun configure-corfu-font ()
  "Configure font for Corfu."
  (when (facep 'corfu-default)  ;; Check if face exists before modifying
    (set-face-attribute 'corfu-bar nil :height 130 :background darkBase :foreground surface1 :weight 'semibold)
    (set-face-attribute 'corfu-default nil :height 130 :background darkBase :foreground surface1 :weight 'semibold)
    (set-face-attribute 'corfu-current nil :height 130 :foreground text :background base :weight 'semibold)
    (set-face-attribute 'corfu-annotations nil :height 130 :foreground surface1 :weight 'semibold)))

;; Run this after Corfu has loaded
(with-eval-after-load 'corfu
  (configure-corfu-font))

;; Also add to server hook if needed
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (featurep 'corfu)
              (configure-corfu-font))))
