;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;; config.el --- Doom Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads modular configuration files from the `configurations/` directory.

;;; Code:

;; ----------------------------------------
;; Doom Emacs Modular Config Loader
;; ----------------------------------------

;; Completion Module
;; Sets up completion frameworks: Company, Vertico, snippets, and related enhancements.
(load! "module/completion-module.el")

;; UI Module
;; Configures visual appearance: themes, modeline, icons, treemacs, dashboard, zen, etc.
(load! "module/ui-module.el")

;; Editor Module
;; Editing behavior: smartparens, multiple-cursors, folds, formatters, snippets, etc.
(load! "module/editor-module.el")

;; Emacs Module
;; Core Emacs extensions: dired, ibuffer, undo, electric, file-templates, etc.
(load! "module/emacs-module.el")

;; Terminal Module
;; Integrated terminals: eshell, vterm, tmux, shell configs (zsh, bash, fish).
(load! "module/term-module.el")

;; Checkers Module
;; Spell checking, grammar, and syntax linting.
(load! "module/checkers-module.el")

;; Tools Module
;; DevOps & productivity: Magit, Docker, Gist, Lookup, Debugger, PDF, Terraform, etc.
(load! "module/tools-module.el")

;; OS Module
;; OS-specific integration: Unix tools, macOS support, TTY handling, direnv, etc.
(load! "module/os-module.el")

;; Language Module
;; Programming language support: LSP, tree-sitter, SQL, web, Ruby, Java, C/C++, etc.
(load! "module/lang-module.el")

;; Email Module
;; Email clients: mu4e setup with org integration and notifications.
(load! "module/email-module.el")

;; App Module
;; Applications: Calendar, RSS, IRC, EMMS, Everywhere, Org-roam, Chat tools.
(load! "module/app-module.el")

;; Config Module
;; User-specific config: personal keybindings, variables, environment, secrets.
(load! "module/config-module.el")

(provide 'config)

;;; config.el ends here
