(use-package astro-ts-mode
  :ensure t
  :config
  ;; Set indentation variables for astro-ts-mode to 4 spaces
  (setq astro-ts-mode-indent-offset 4)
  (add-hook 'astro-ts-mode-hook
            (lambda ()
              ;; For astro specific indentation
              (setq-local astro-ts-mode-indent-offset 4)
              ;; For TypeScript/JSX parts
              (setq-local typescript-ts-mode-indent-offset 4)
              ;; For CSS parts
              (setq-local css-indent-offset 4))))

(add-hook 'astro-ts-mode-hook 'lsp-deferred)
;; ;; Make sure web-mode is installed first
;; (use-package web-mode
;;   :ensure t)

;; ;; Define astro-mode based on web-mode
;; (define-derived-mode astro-mode web-mode "Astro"
;;   "Major mode for editing Astro files."
;;   ;; Set the language ID for LSP
;;   (setq-local lsp-language-id-configuration '((astro-mode . "astro")))
  
;;   ;; Configure web-mode for Astro
;;   (setq-local web-mode-markup-indent-offset 2)
;;   (setq-local web-mode-css-indent-offset 2)
;;   (setq-local web-mode-code-indent-offset 2)
;;   (setq-local web-mode-script-padding 2)
;;   (setq-local web-mode-style-padding 2)
  
;;   ;; Enable auto-completion
;;   (setq-local web-mode-enable-auto-pairing t)
;;   (setq-local web-mode-enable-css-colorization t)
  
;;   ;; Enable JavaScript highlighting features
;;   (setq-local web-mode-enable-block-face t)
;;   (setq-local web-mode-enable-part-face t)
;;   (setq-local web-mode-enable-comment-interpolation t)
;;   (setq-local web-mode-enable-string-interpolation t)
  
;;   ;; Critical frontmatter settings
;;   (setq-local web-mode-enable-front-matter-highlight t)
;;   (setq-local web-mode-enable-front-matter-block t)
  
;;   ;; Set content types for proper highlighting
;;   (setq-local web-mode-content-types-alist
;;         '(("jsx" . "\\.astro\\'")
;;           ("javascript" . "\\.astro\\'")))
  
;;   ;; Configure engine for Astro components
;;   (setq-local web-mode-engine "astro")
  
;;   ;; Enhanced JavaScript highlighting in script sections
;;   (setq-local web-mode-script-types
;;               '(("jsx" . "\\.astro\\'")
;;                 ("tsx" . "\\.astro\\'")))
  
;;   ;; Define frontmatter specifically for Astro
;;   (setq-local web-mode-comment-formats '(("javascript" . "//")
;;                                          ("jsx" . "//")
;;                                          ("astro" . "//")))
  
;;   ;; Add custom highlighting for Astro frontmatter and other elements
;;   (font-lock-add-keywords
;;    nil
;;    '(;; Highlight the frontmatter delimiters distinctly
;;      ("^---" 0 'font-lock-preprocessor-face)
     
;;      ;; Astro global
;;      ("\\<\\(Astro\\)\\>" . font-lock-type-face)
     
;;      ;; Props and context
;;      ("\\<\\(Astro\\.props\\|Astro\\.request\\|Astro\\.url\\|Astro\\.site\\)\\>" . font-lock-variable-name-face))))

;; ;; Associate .astro files with astro-mode
;; (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode))

;; ;; Configure web-mode specifically for Astro
;; (with-eval-after-load 'web-mode
;;   ;; Add astro as an engine
;;   (add-to-list 'web-mode-engines-alist '("astro" . "\\.astro\\'"))
  
;;   ;; Configure highlighting for JavaScript sections in Astro files
;;   (add-to-list 'web-mode-content-types-alist '("jsx" . "\\.astro\\'"))
;;   (add-to-list 'web-mode-content-types-alist '("javascript" . "\\.astro\\'"))
  
;;   ;; Define frontmatter delimiters specifically for Astro
;;   (add-to-list 'web-mode-engine-token-regexps '("astro" . "^---$"))
  
;;   ;; Enhance JavaScript highlighting in all contexts
;;   (setq web-mode-javascript-font-lock-keywords
;;         (append web-mode-javascript-font-lock-keywords
;;                 '(("\\<\\(async\\|await\\|of\\|as\\|from\\|import\\|export\\|const\\|let\\|var\\|if\\|else\\|for\\|while\\|do\\|switch\\|case\\|default\\|break\\|continue\\|return\\|function\\|class\\|extends\\|super\\|this\\|new\\|try\\|catch\\|finally\\|throw\\|typeof\\|instanceof\\|in\\|yield\\)\\>" . font-lock-keyword-face)
;;                   ("\\<\\(true\\|false\\|null\\|undefined\\)\\>" . font-lock-constant-face)
;;                   ("\\<\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*:" . font-lock-variable-name-face)
;;                   ("`[^`]*`" . font-lock-string-face)))))

;; ;; Extra functions to ensure proper highlighting

;; ;; Force rehighlighting of the entire buffer
;; (defun astro-rehighlight-buffer ()
;;   "Rehighlight the entire buffer to ensure proper highlighting in Astro files."
;;   (interactive)
;;   (if (derived-mode-p 'astro-mode)
;;       (progn
;;         (font-lock-flush)
;;         (font-lock-ensure))))

;; ;; Define the function to improve frontmatter highlighting
;; (defun astro-highlight-frontmatter ()
;;   "Enhance frontmatter highlighting in Astro files."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (looking-at "^---$")
;;       (let ((start (point))
;;             (end (progn (forward-line) (search-forward "---" nil t))))
;;         (when end
;;           ;; Apply JavaScript highlighting to the frontmatter region
;;           (font-lock-add-keywords nil
;;            '(("\\<\\(import\\|export\\|from\\|const\\|let\\|var\\|if\\|else\\|for\\|of\\|in\\|function\\|return\\|async\\|await\\)\\>" . font-lock-keyword-face)
;;              ("\\<\\(true\\|false\\|null\\|undefined\\)\\>" . font-lock-constant-face)
;;              ("`[^`]*`" . font-lock-string-face)
;;              ("\\<\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*:" . font-lock-variable-name-face)
;;              ("\\<function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" . font-lock-function-name-face))
;;            t start end))))))

;; ;; Astro LSP Configuration - Add this separately from your main astro-mode file
;; ;; Make sure lsp-mode and the required packages are available
