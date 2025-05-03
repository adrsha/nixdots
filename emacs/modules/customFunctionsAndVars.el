(load-file "~/.config/emacs/modules/colors.el")

(defvar cust-monospace "JetBrainsMono Nerd Font"
  "The monospace font for emacs.")
(defvar cust-serif "Autour One"
  "The serif font for emacs.")
(defvar cust-sans-serif "Inter"
  "The sans font for emacs.")

;; "Ndot77JPExtended"

(defun delete-window-or-frame (&optional window frame force)
  "Delete WINDOW or FRAME if it's the only window.
If WINDOW is the only one in FRAME, delete FRAME instead.
When called interactively, delete the selected window or the selected frame."
  (interactive)
  (let ((win (or window (selected-window)))
        (f (or frame (selected-frame))))
    (if (= 1 (length (window-list f)))
        (delete-frame f force)
      (delete-window win))))

(defun clear ()
  "Clear display, return to normal state, and quit iedit mode if active."
  (interactive)
  (when (bound-and-true-p iedit-mode)
    (iedit--quit))
  (redraw-display)
  (when (fboundp 'evil-force-normal-state)
    (evil-force-normal-state)))

;; Evil mode hooks consolidated with better naming
(defun my-enable-evil-escape-mode ()
  "Enable evil-escape-mode."
  (when (fboundp 'evil-escape-mode)
    (evil-escape-mode 1)))

(defun my-disable-evil-escape-mode ()
  "Disable evil-escape-mode."
  (when (fboundp 'evil-escape-mode)
    (evil-escape-mode -1)))

;; Use lexical binding for the hooks
(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-entry-hook #'my-enable-evil-escape-mode)
  (add-hook 'minibuffer-setup-hook #'my-enable-evil-escape-mode)
  (add-hook 'evil-visual-state-entry-hook #'my-disable-evil-escape-mode)
  (add-hook 'org-agenda-mode-hook #'my-disable-evil-escape-mode))

;; Better buffer navigation with improved regexp handling
(defcustom my-skippable-buffer-regexp
  (rx bos (or 
           ;; Common buffers to skip
           "*Messages*" "*scratch*" "*Help*"
           ;; Any buffer starting with *
           (seq "*" (zero-or-more anything)))
      eos)
  "Matching buffer names are ignored by `my-next-buffer' and `my-previous-buffer'."
  :type 'regexp
  :group 'convenience)

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until `my-skippable-buffer-regexp' doesn't match.
Avoids infinite loops by tracking visited buffers."
  (let ((initial (current-buffer))
        (visited-buffers (make-hash-table :test 'eq)))
    (puthash initial t visited-buffers)
    (funcall change-buffer)
    
    (while (and (string-match-p my-skippable-buffer-regexp (buffer-name))
                (not (gethash (current-buffer) visited-buffers)))
      (puthash (current-buffer) t visited-buffers)
      (funcall change-buffer))
    
    ;; If we've looped through all buffers and they all match the skip pattern
    (when (gethash (current-buffer) visited-buffers)
      (switch-to-buffer initial))))

(defun my-next-buffer ()
  "Variant of `next-buffer' that skips buffers matching `my-skippable-buffer-regexp'."
  (interactive)
  (my-change-buffer #'next-buffer))

(defun my-previous-buffer ()
  "Variant of `previous-buffer' that skips buffers matching `my-skippable-buffer-regexp'."
  (interactive)
  (my-change-buffer #'previous-buffer))

(defun read-from-file (file)
  "Read Lisp expression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun open-current-file-in-vim ()
  "Open current file in Neovim at the current position."
  (interactive)
  (if buffer-file-name
      (let ((line-number (line-number-at-pos (point))))
        (start-process "nvim" nil "alacritty" "-e" "nvim" 
                       (concat "+" (number-to-string line-number)) 
                       (shell-quote-argument buffer-file-name)))
    (message "Buffer is not visiting a file")))

(defun open-in-terminal ()
  "Open current directory in terminal using Hyprland's special workspace."
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (quoted-dir (shell-quote-argument dir))
         (terminal-cmd nil))
    
    ;; Find available terminal
    (cond
     ((executable-find "alacritty")
      (setq terminal-cmd "alacritty"))
     ((executable-find "kitty")
      (setq terminal-cmd "kitty"))
     ((executable-find "foot")
      (setq terminal-cmd "foot"))
     ((executable-find "wezterm")
      (setq terminal-cmd "wezterm")))
    
    (if terminal-cmd
        (let ((hypr-cmd (format 
                         "hyprctl dispatch exec '[workspace special:magic] %s --working-directory=%s'"
                         terminal-cmd quoted-dir)))
          (start-process-shell-command "hyprland-term" nil hypr-cmd))
      (message "No supported terminal emulator found"))))

(defun rename-current-buffer-file ()
  "Rename current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer '%s' is not visiting a file!" (buffer-name)))
    
    (let* ((dir (file-name-directory filename))
           (new-name (read-file-name "New name: " dir)))
      
      ;; Check for existing buffer with target name
      (when (get-buffer (file-name-nondirectory new-name))
        (user-error "A buffer named '%s' already exists!" (file-name-nondirectory new-name)))
      
      ;; Create directory if needed
      (let ((new-dir (file-name-directory new-name)))
        (when (and (not (file-exists-p new-dir))
                   (yes-or-no-p (format "Create directory '%s'? " new-dir)))
          (make-directory new-dir t)))
      
      ;; Perform the rename operation
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer (file-name-nondirectory new-name))
      (set-buffer-modified-p nil)
      
      ;; Update recentf if available
      (when (fboundp 'recentf-add-file)
        (recentf-add-file new-name)
        (recentf-remove-if-non-kept filename))
      
      (message "File renamed to '%s'" (file-name-nondirectory new-name)))))

(defun hm/convert-org-to-docx-with-pandoc ()
  "Use Pandoc to convert current Org file to DOCX with timestamp."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  
  (let* ((input-file (expand-file-name buffer-file-name))
         (output-file (concat (file-name-sans-extension input-file)
                              (format-time-string "-%Y-%m-%d-%H%M%S")
                              ".docx"))
         (cmd (format "pandoc -N --from org \"%s\" -o \"%s\"" 
                      input-file output-file)))
    (message "Converting to DOCX: %s" output-file)
    (let ((exit-code (call-process-shell-command cmd nil "*Pandoc Output*" t)))
      (if (= exit-code 0)
          (message "Export successful: %s" (file-name-nondirectory output-file))
        (pop-to-buffer "*Pandoc Output*")
        (message "Export failed with exit code %d" exit-code)))))

(defun er-open-asm (arg)
  "Open visited file in default external program.
With prefix ARG, prompt for command to use."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  
  (let ((command
         (if arg
             (read-shell-command "Open current file with: ")
           (pcase system-type
             ('darwin "open")
             ((or 'gnu 'gnu/linux 'gnu/kfreebsd) "xdg-open")
             (_ (read-shell-command "Open current file with: "))))))
    (start-process "external-open" nil command buffer-file-name)))

(defun compile-latex-doc ()
  "Compile current LaTeX document with pdflatex."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  
  (unless (file-exists-p buffer-file-name)
    (save-buffer))
  
  (let ((default-directory (file-name-directory buffer-file-name))
        (file-name (file-name-nondirectory buffer-file-name))
        (process-buffer "*LaTeX Compilation*"))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (with-current-buffer (get-buffer-create process-buffer)
      (erase-buffer)
      (compilation-mode)
      (message "Compiling LaTeX document...")
      (if (= 0 (call-process "pdflatex" nil process-buffer t "-interaction=nonstopmode" file-name))
          (message "LaTeX compilation finished successfully")
        (pop-to-buffer process-buffer)
        (message "LaTeX compilation failed")))))

(defun google-this ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Google: "))))
    (browse-url
     (concat "https://www.google.com/search?q=" 
             (url-hexify-string query)))))

(defun org-schedule-tomorrow ()
  "Schedule the current Org item for tomorrow."
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-schedule nil "+1d")
    (user-error "Not in org-mode")))

(defun org-copy-blocks ()
  "Copy all source blocks in current subtree to kill ring."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((code-blocks ""))
        (save-restriction
          (org-narrow-to-subtree)
          (org-babel-map-src-blocks nil
            (setq code-blocks (concat code-blocks 
                                      (when (> (length code-blocks) 0) "\n\n")
                                      (org-no-properties body)))))
        (if (string-empty-p code-blocks)
            (message "No source blocks found in subtree")
          (kill-new code-blocks)
          (message "Source blocks copied to kill ring")))
    (user-error "Not in org-mode")))

;;; Additional useful functions

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line or region N times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (buffer-substring (line-beginning-position)
                                      (line-end-position)))))
        (dotimes (_ (or n 1))
          (if use-region
              ;; When using a region
              (goto-char (region-end))
            ;; When using a single line
            (end-of-line))
          (newline)
          (insert text))))
    ;; Move cursor to the second instance if not using a region
    (when (not use-region)
      (let ((column (current-column)))
        (forward-line)
        (move-to-column column)))))

(defun toggle-window-split ()
  "Toggle between horizontal and vertical window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-timestamp ()
  "Insert current timestamp at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))


(defun copy-file-path ()
  "Copy the current buffer file path to kill ring."
  (interactive)
  (if buffer-file-name
      (let ((path (buffer-file-name)))
        (kill-new path)
        (message "Copied: %s" path))
    (message "Buffer is not visiting a file")))

(defun copy-file-directory ()
  "Copy the directory of current buffer to kill ring."
  (interactive)
  (if buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (kill-new dir)
        (message "Copied: %s" dir))
    (message "Buffer is not visiting a file")))


(defun insert-uuid ()
  "Insert a UUID at point."
  (interactive)
  (if (executable-find "uuidgen")
      (insert (string-trim (shell-command-to-string "uuidgen")))
    ;; Fallback implementation when uuidgen isn't available
    (let ((uuid (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
                        (random 65536) (random 65536) (random 65536)
                        (logior 16384 (logand (random 65536) 16383))
                        (logior 32768 (logand (random 65536) 16383))
                        (random 65536) (random 65536) (random 65536))))
      (insert uuid))))

(defun unfill-paragraph ()
  "Convert a paragraph's lines to a single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (beg end)
  "Unfill each paragraph in the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph)
      (forward-paragraph 1))))

(defun evil-visual-line-to-normal-end-of-line ()
  "Make $ work like in Vim when in visual line mode."
  (interactive)
  (evil-end-of-line))

(defun toggle-evil-hybrid-mode ()
  "Toggle between Evil normal and Emacs state for quick editing."
  (interactive)
  (if (evil-normal-state-p)
      (evil-emacs-state)
    (evil-normal-state)))

(defun evil-narrow-to-defun ()
  "Narrow to current function, works with Evil."
  (interactive)
  (save-excursion
    (evil-normal-state)
    (mark-defun)
    (narrow-to-region (region-beginning) (region-end))))

(defun evil-widen ()
  "Widen buffer from narrowed state, preserving evil state."
  (interactive)
  (let ((state (evil-state)))
    (widen)
    (evil-change-state state)))

(defun evil-clear-search-highlight ()
  "Clear search highlight in Evil mode."
  (interactive)
  (evil-ex-nohighlight)
  (force-mode-line-update))

(defun evil-replace-in-buffer (old new)
  "Replace OLD with NEW throughout buffer, preserving Evil state."
  (interactive "sReplace: \nsReplace with: ")
  (let ((state (evil-state)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward old nil t)
        (replace-match new)))
    (evil-change-state state)))

(defun evil-search-word-under-cursor ()
  "Search for the word under cursor without moving."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (evil-search (regexp-quote word) t t)))

(defun toggle-evil-global-markers-in-project ()
  "Toggle between project-only and global Evil file marks."
  (interactive)
  (if (bound-and-true-p evil-global-markers-in-project)
      (progn
        (setq evil-global-markers-in-project nil)
        (message "Using global file marks"))
    (setq evil-global-markers-in-project t)
    (message "Using project-only file marks")))

(defun jump-to-register-other-window (register)
  "Jump to register in another window."
  (interactive (list (register-read-with-preview "Jump to register in other window: ")))
  (split-window)
  (other-window 1)
  (jump-to-register register))

(defun evil-macro-on-matching-lines (regex kbd-macro)
  "Apply keyboard macro to all lines matching REGEX."
  (interactive "sRegex: \nXKeyboard macro: ")
  (evil-normal-state)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (beginning-of-line)
      (execute-kbd-macro kbd-macro)
      (forward-line 1))))

(defun evil-align-by-regexp (regexp)
  "Align text by REGEXP in selected region, Evil style."
  (interactive "sAlign by: ")
  (when (region-active-p)
    (align-regexp (region-beginning) (region-end) 
                  (concat "\\(\\s-*\\)" regexp) 1 1 t)))

(defun center-current-line ()
  "Center the current line in window."
  (interactive)
  (let ((recenter-positions '(middle)))
    (recenter-top-bottom)))

(defun dired-get-size ()
  "Get size of marked files in Dired."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-sch" files)
      (message "Size of %d files: %s"
               (length files)
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(defun edit-init-file ()
  "Edit init.el file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun load-init-file ()
  "Load init.el file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun sudo-edit (&optional arg)
  "Edit file with sudo. With ARG, prompt for filename."
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file (concat "/sudo::" (expand-file-name fname)))))

(defun insert-lorem ()
  "Insert lorem ipsum text."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."))

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring based on CHOICE.
CHOICE can be:
1 = full path
2 = directory
3 = filename
4 = filename without extension"
  (interactive "cCopy: [1] full-path, [2] directory, [3] filename, [4] name only ")
  (if buffer-file-name
      (let* ((name (buffer-file-name))
             (result (cond
                      ((eq choice ?1) name)
                      ((eq choice ?2) (file-name-directory name))
                      ((eq choice ?3) (file-name-nondirectory name))
                      ((eq choice ?4) (file-name-base name))
                      (t name))))
        (kill-new result)
        (message "Copied: %s" result))
    (message "Buffer is not visiting a file")))

(defun create-scratch-buffer ()
  "Create a scratch buffer with same mode as current buffer."
  (interactive)
  (let* ((mode major-mode)
         (buf (generate-new-buffer "*scratch*")))
    (with-current-buffer buf
      (funcall mode)
      (setq header-line-format 
            (format "Scratch buffer for %s. C-x k to kill." 
                    (symbol-name mode))))
    (switch-to-buffer buf)))

(defun evil-retab ()
  "Convert between tabs and spaces according to tab-width."
  (interactive)
  (let* ((state (evil-state))
         (use-tabs indent-tabs-mode)
         (tab-size tab-width))
    (untabify (point-min) (point-max))
    (when use-tabs
      (tabify (point-min) (point-max)))
    (evil-change-state state)))

(defun evil-goto-matching-paren ()
  "Jump to the matching parenthesis."
  (interactive)
  (let ((orig-point (point)))
    (when (cond
           ((looking-at "[][(){}]") (forward-char 1))
           ((looking-back "[][(){}]" 1) t)
           (t nil))
      (backward-char 1)
      (cond
       ((looking-at "\\s(") (forward-list 1) (backward-char 1))
       ((looking-at "\\s)") (forward-char 1) (backward-list 1))
       (t (goto-char orig-point))))))

(defun org-dwim-heading-or-item ()
  "Create a heading or list item intelligently."
  (interactive)
  (cond
   ((org-at-item-p) (org-insert-item))
   ((org-at-heading-p) (org-insert-heading-after-current))
   (t (org-insert-heading))))

(defun toggle-transparency ()
  "Toggle frame transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (or (not alpha) (= (cdr-safe alpha) 100))
        (set-frame-parameter nil 'alpha '(85 . 75))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(defun increase-font-size ()
  "Increase font size."
  (interactive)
  (let ((new-height (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)))

(defun decrease-font-size ()
  "Decrease font size."
  (interactive)
  (let ((new-height (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)))

(defun toggle-frame-fullscreen-tiling ()
  "Toggle fullscreen that works with tiling WMs."
  (interactive)
  (let ((frame (selected-frame)))
    (modify-frame-parameters 
     frame 
     `((fullscreen . ,(if (eq (frame-parameter frame 'fullscreen) 'fullboth)
                          nil 'fullboth))))))

(defun custom-evil-shift-right ()
  "Shift right with a single `>` key press."
  (interactive)
  (if (use-region-p)
      (evil-shift-right (region-beginning) (region-end))
    (evil-shift-right (line-beginning-position) (line-end-position))))

(defun custom-evil-shift-left ()
  "Shift right with a single `<` key press."
  (interactive)
  (if (use-region-p)
      (evil-shift-left (region-beginning) (region-end))
    (evil-shift-left (line-beginning-position) (line-end-position))))

(defun smart-align-chars (start end char)
  "Align region (START to END) on the first occurrence of CHAR in each line.
Example usage for aligning on equals signs:
  M-x align-region-on-char RET = RET

For a selected region like:
  let x = 10;
  let longVariable = 20;
  let z = 30;

Will produce:
  let x            = 10;
  let longVariable = 20;
  let z            = 30;"

  (interactive "r\ncAlign on character: ")
  (let ((char-string (char-to-string char))
        (max-col 0)
        positions)
    
    ;; First pass: find positions of the character in each line and max position
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position))
              pos)
          (save-excursion
            (when (search-forward char-string line-end t)
              (setq pos (- (point) line-start 1))
              (setq max-col (max max-col pos))
              (push (cons (line-number-at-pos) pos) positions)))
          (forward-line 1))))
    
    ;; Second pass: add spaces to align
    (save-excursion
      (dolist (pos-pair positions)
        (goto-char start)
        (forward-line (- (car pos-pair) (line-number-at-pos start)))
        (move-to-column (cdr pos-pair))
        (let ((spaces-needed (- max-col (cdr pos-pair))))
          (when (> spaces-needed 0)
            (insert (make-string spaces-needed ? ))))))))


(defun toggle-split-join ()
  "Toggle between split and joined forms of code structures using tree-sitter.
Similar to TreeSJ in Neovim but for Emacs."
  (interactive)
  (if (not (treesit-available-p))
      (message "Tree-sitter not available or not enabled in this buffer")
    (let* ((node (treesit-node-at (point)))
           (container-node nil)
           (start nil)
           (end nil)
           (content nil)
           (is-split nil))
      
      ;; Find containing node - focus on object patterns and similar structures
      (setq container-node
            (or (treesit-parent-until 
                 node
                 (lambda (n)
                   (let ((type (treesit-node-type n)))
                     (or (string-match-p "\\(object\\|array\\|arguments\\|parameters\\)" type)
                         (and (member type '("object_pattern" "array_pattern" 
                                             "formal_parameters" "arguments"))
                              (not (string-match-p "statement\\|declaration" type)))))))
                node))
      
      (when container-node
        ;; Get node boundaries
        (setq start (treesit-node-start container-node))
        (setq end (treesit-node-end container-node))
        
        ;; Extract content
        (setq content (buffer-substring-no-properties start end))
        
        ;; Check if it's already split (contains newlines between elements)
        (setq is-split (string-match-p "\n" content))
        
        ;; Transform content based on current state
        (if is-split
            ;; Join lines
            (join-node-content container-node)
          ;; Split lines
          (split-node-content container-node))))))

(defun join-node-content (node)
  "Join multi-line node content into a single line."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (content (buffer-substring-no-properties 
                  (treesit-node-start node) 
                  (treesit-node-end node))))
    ;; Replace newlines and their surrounding whitespace with a single space
    (let ((joined-content (replace-regexp-in-string 
                           "[ \t]*\n[ \t]*" " " 
                           content)))
      ;; Normalize spaces (no multiple spaces)
      (setq joined-content (replace-regexp-in-string "[ \t]+" " " joined-content))

      ;; Remove space after opening delimiter
      (setq joined-content (replace-regexp-in-string 
                            "\\([{(\\[]\\) " 
                            "\\1" 
                            joined-content))
      ;; Remove space before closing delimiter
      (setq joined-content (replace-regexp-in-string 
                            " \\([})\\]]\\)" 
                            "\\1" 
                            joined-content))
      
      ;; Replace region with joined content
      (delete-region start end)
      (goto-char start)
      (insert joined-content))))

(defun split-node-content (node)
  "Split node content into multiple lines with proper indentation."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (content (buffer-substring-no-properties start end))
         (node-type (treesit-node-type node))
         (elements nil)
         (processed nil))
    
    ;; Handle different node types
    (setq elements 
          (cond
           ;; For object patterns/literals (destructuring, object literals)
           ((string-match-p "object" node-type)
            (treesit-node-children node (lambda (child) 
                                          (member (treesit-node-type child)
                                                  '("pair" "property" "spread_element" 
                                                    "object_property" "property_identifier"
                                                    "shorthand_property_identifier")))))
           
           ;; Arrays and lists
           ((string-match-p "\\(array\\|list\\)" node-type)
            (treesit-node-children node (lambda (child)
                                          (not (member (treesit-node-type child)
                                                       '("," "[" "]" "{" "}" "(" ")"))))))
           
           ;; Function parameters and arguments
           ((string-match-p "\\(parameters\\|arguments\\)" node-type)
            (treesit-node-children node (lambda (child)
                                          (not (member (treesit-node-type child)
                                                       '("," "(" ")" "[" "]" "{" "}")))))))
          
          ;; If no elements found (treesit structure unclear), use regex splitting
          (if (or (null elements) (= (length elements) 0))
              (progn
                (let ((cleaned-content (substring content 1 -1)))  ;; Remove delimiters
                  (delete-region start end)
                  (goto-char start)
                  (insert (concat (substring content 0 1) "\n"
                                  (mapconcat (lambda (elem) 
                                               (concat "  " (string-trim elem)))
                                             (split-string cleaned-content ",")
                                             ",\n")
                                  "\n" (substring content -1)))
                  (indent-region start (point))
                  (goto-char start)
                  (setq processed t)))
            
            ;; Process elements if found and not already processed
            (unless processed
              (let ((opener (substring content 0 1))
                    (closer (substring content -1))
                    (indent-level (+ (current-indentation) 2))
                    (split-content ""))
                
                (delete-region start end)
                (goto-char start)
                
                ;; Create the split content
                (setq split-content 
                      (concat opener "\n"
                              (mapconcat 
                               (lambda (elem)
                                 (concat (make-string indent-level ? )
                                         (string-trim (buffer-substring-no-properties 
                                                       (treesit-node-start elem)
                                                       (treesit-node-end elem)))))
                               elements
                               ",\n")
                              "\n" (make-string (current-indentation) ? ) closer))
                
                (insert split-content)
                (indent-region start (point))))))))

;; Utility function to help navigate up the syntax tree
(defun treesit-parent-until (node predicate)
  "Return the nearest ancestor of NODE that satisfies PREDICATE."
  (let ((current node)
        (result nil))
    (while (and current (not result))
      (setq current (treesit-node-parent current))
      (when (and current (funcall predicate current))
        (setq result current)))
    result))
