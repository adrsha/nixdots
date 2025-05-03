(use-package which-key
             :ensure t
             :config
             ;; Set the time delay (in seconds) for the which-key popup to appear. A value of
             ;; zero might cause issues so a non-zero value is recommended.
             (setq which-key-idle-delay 0.3)

             ;; Set the maximum length (in characters) for key descriptions (commands or
             ;; prefixes). Descriptions that are longer are truncated and have ".." added.
             ;; This can also be a float (fraction of available width) or a function.
             (setq which-key-max-description-length 27)

             ;; Use additional padding between columns of keys. This variable specifies the
             ;; number of spaces to add to the left of each column.
             (setq which-key-add-column-padding 3)

             ;; The maximum number of columns to display in the which-key buffer. nil means
             ;; don't impose a maximum.
             (setq which-key-max-display-columns nil)

             ;; Set the separator used between keys and descriptions. Change this setting to
             ;; an ASCII character if your font does not show the default arrow. The second
             ;; setting here allows for extra padding for Unicode characters. which-key uses
             ;; characters as a means of width measurement, so wide Unicode characters can
             ;; throw off the calculation.
             (setq which-key-separator "  " )

             ;; Set the prefix string that will be inserted in front of prefix commands
             ;; (i.e., commands that represent a sub-map).
             (setq which-key-prefix-prefix " " )

             ;; Set the special keys. These are automatically truncated to one character and
             ;; have which-key-special-key-face applied. Disabled by default. An example
             ;; setting is
             ;; (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
             (setq which-key-special-keys nil)

             ;; Show the key prefix on the left, top, or bottom (nil means hide the prefix).
             ;; The prefix consists of the keys you have typed so far. which-key also shows
             ;; the page information along with the prefix.
             (setq which-key-show-prefix 'nil)

             ;; Set to t to show the count of keys shown vs. total keys in the mode line.
             (setq which-key-show-remaining-keys nil)

             (setq which-key-frame-max-height 10)

             (setq which-key-frame-max-width 150)

             (setq which-key-popup-type 'frame)

             (which-key-mode))
