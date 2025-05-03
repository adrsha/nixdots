(defun my-valid-capf (capf)
  "Wrap a CAPF to ensure it returns a valid list."
  (lambda ()
    (let ((res (ignore-errors funcall capf)))
      (when (and res (listp res)) res))))

(use-package codeium
  :ensure t
  :vc (:url "https://github.com/Exafunction/codeium.el")
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (codeium-init)
              (add-hook 'completion-at-point-functions
                        (cape-capf-super (my-valid-capf #'codeium-completion-at-point)
                                         (my-valid-capf #'lsp-completion-at-point))
                        nil t)))
  :config
  (setq use-dialog-box nil)
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min))
                                    (min (+ (point) 1000) (point-max))))
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
