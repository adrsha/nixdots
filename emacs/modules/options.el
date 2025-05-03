(setq-default
 ad-redefinition-action 'accept                  ; Silence warnings for redefinition
 delete-by-moving-to-trash t                     ; Delete files to trash
 help-window-select t                            ; Focus new help windows when opened
 mouse-yank-at-point t                           ; Yank at point rather than cursor
 scroll-conservatively most-positive-fixnum      ; Always scroll by one line
 select-enable-clipboard t                       ; Merge system's and Emacs' clipboard
 show-trailing-whitespace nil                    ; Do not display trailing whitespaces
 tab-width 2                                     ; Set width for tabs
 uniquify-buffer-name-style 'forward             ; Uniquify buffer names
 ring-bell-function 'ignore                      ; Be quiet!
 custom-file (locate-user-emacs-file "custom-vars.el")    ; Move customization variables to a separate file and load it
 confirm-kill-processes nil
 sentence-end-double-space nil               ; a sentence ends with only one space
 scroll-step 1                               ; Line wise scroll.
 scroll-conservatively 101                   ; Whether to recenter cursor on scroll. If the value is greater than 100, it wont.
 indent-tabs-mode nil
 tab-always-indent 't
 css-fontify-colors nil
 user-emacs-directory (expand-file-name "~/.cache/emacs")
 recentf-max-menu-items 25
 recentf-max-saved-items 25
 recentf-save-file "~/.cache/emacs/recentf"
 native-comp-async-report-warnings-errors nil
 warning-minimum-level :emergency
 echo-keystrokes 0.0
 evil-echo-state nil
 widget-image-enable nil
 tab-width 2
 dired-use-ls-dired nil
 dired-kill-when-opening-new-dired-buffer t
 evil-want-keybinding nil
 )     

(if (boundp 'comp-deferred-compilation)
    (setq-default comp-deferred-compilation nil)
  (setq-default native-comp-deferred-compilation nil))
(load custom-file 'noerror 'nomessage)

(setq
 global-auto-revert-non-file-buffers t        ; Auto revert other non file buffers too
 package-enable-at-startup nil
 ;;byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 byte-compile-warnings 'nil
 backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
 )      

(fset 'yes-or-no-p 'y-or-n-p)                      ; Replace yes/no prompts with y/n
(recentf-mode 1)                                   ; Remember recently opened files
(save-place-mode 1)                                ; Remember the last traversed point in file
(global-auto-revert-mode 1)                        ; Automatically revert buffers when the underlying file is changed

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil) 
(put 'dired-find-alternate-file 'disabled nil)     ; Open dired in same buffer

