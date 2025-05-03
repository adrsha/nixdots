;; Initial Setup
(load-file "~/.config/emacs/modules/disableUI.el");
(load-file "~/.config/emacs/modules/options.el");
(load-file "~/.config/emacs/modules/packageManager.el");
(load-file "~/.config/emacs/modules/customFunctionsAndVars.el");
(load-file "~/.config/emacs/modules/advicesAndHooks.el"); 

;; Installing Packages
(load-file "~/.config/emacs/modules/packages/tryPackages.el");
(load-file "~/.config/emacs/modules/packages/evilMode.el");
(load-file "~/.config/emacs/modules/packages/undo.el");
(load-file "~/.config/emacs/modules/packages/docs.el");
(load-file "~/.config/emacs/modules/packages/general.el");
(load-file "~/.config/emacs/modules/packages/gc.el");
(load-file "~/.config/emacs/modules/packages/whichkey.el");
(load-file "~/.config/emacs/modules/packages/vertico.el");
(load-file "~/.config/emacs/modules/packages/icons.el");
(load-file "~/.config/emacs/modules/packages/accessories.el");
(load-file "~/.config/emacs/modules/packages/snippets.el");
(load-file "~/.config/emacs/modules/packages/theme.el");
(load-file "~/.config/emacs/modules/packages/org.el");
(load-file "~/.config/emacs/modules/packages/lsp.el");
(load-file "~/.config/emacs/modules/packages/corfu.el");
(load-file "~/.config/emacs/modules/packages/ai.el");
(load-file "~/.config/emacs/modules/packages/fileManagement.el");

;; Customizing looks
(load-file "~/.config/emacs/modules/setFaces.el");

;; Treesitter
(load-file "~/.config/emacs/modules/treesit.el");

;; Custom modes
(load-file "~/.config/emacs/modules/astro.el");

;; Custom packages
(load-file "~/.config/emacs/modules/echobar.el");
