;; Syntax Highlighting with Tree-sitter
(require 'treesit)

;; Define all language sources
;; Shell
(add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))

;; Python
(add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python"))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; C/C++
(add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(add-to-list 'treesit-language-source-alist '(cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

;; Web development languages
(add-to-list 'treesit-language-source-alist '(css "https://github.com/tree-sitter/tree-sitter-css"))
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))

(add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html"))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

;; JavaScript and JSX
(add-to-list 'treesit-language-source-alist '(javascript "https://github.com/tree-sitter/tree-sitter-javascript"))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))

;; TypeScript and TSX
(add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
(add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))

(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(tsx-mode . tsx-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Set syntax highlighting level
(setq treesit-font-lock-level 4)

;; Install all the language grammars automatically if they're missing
(defun my/install-treesit-languages ()
  "Install all tree-sitter language grammars defined in treesit-language-source-alist."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (message "Installing tree-sitter grammar for %s" lang)
        (treesit-install-language-grammar lang)))))

;; Run the installation function
(my/install-treesit-languages)
