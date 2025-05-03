
(use-package catppuccin-theme
             :ensure t
             :config
             (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
             (load-theme 'catppuccin :no-confirm)

             ;; Apply colors using the global variables
             (catppuccin-set-color 'rosewater rosewater)
             (catppuccin-set-color 'flamingo flamingo)
             (catppuccin-set-color 'pink pink)
             (catppuccin-set-color 'mauve mauve)
             (catppuccin-set-color 'red red)
             (catppuccin-set-color 'maroon maroon)
             (catppuccin-set-color 'peach peach)
             (catppuccin-set-color 'yellow yellow)
             (catppuccin-set-color 'green green)
             (catppuccin-set-color 'teal teal)
             (catppuccin-set-color 'sky sky)
             (catppuccin-set-color 'sapphire sapphire)
             (catppuccin-set-color 'blue blue)
             (catppuccin-set-color 'lavender lavender)
             (catppuccin-set-color 'text text)
             (catppuccin-set-color 'subtext1 subtext1)
             (catppuccin-set-color 'subtext0 subtext0)
             (catppuccin-set-color 'overlay2 overlay2)
             (catppuccin-set-color 'overlay1 overlay1)
             (catppuccin-set-color 'overlay0 overlay0)
             (catppuccin-set-color 'surface2 surface2)
             (catppuccin-set-color 'surface1 surface1)
             (catppuccin-set-color 'surface0 surface0)
             (catppuccin-set-color 'mantle mantle)
             (catppuccin-set-color 'crust crust)
             (catppuccin-set-color 'base base)

             (catppuccin-reload))
