;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Borislav Dostumski"
      user-mail-address "b.dostumski@gmail.com")

;; yast snippets
(setq yas-snippet-dirs
      '("~/.config/emacs/snippets"))

;; Auto install missing packages
(setq use-package-always-ensure t)

;; Enable projecticle for main workspace directory
(setq projectile-project-search-path '("~/Workspace/"))
;; Auto-discover projects
(setq projectile-auto-discover t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads
;; It enables you to quickly jot down thoughts and easily retrieve them later.
(load! "packages/ui/deft.el")

;; Make Doom fabulous again
(load! "packages/ui/doom.el")

;; This module provides an Atom-inspired, minimalistic modeline for Doom Emacs
(load! "packages/ui/modeline.el")

;; Enable (evil-goggles-mode), then edit text like you normally would, try for example yy, p, dd in normal state.
(load! "packages/ui/ophints.el")

;; Treemacs is a file and project explorer similar to NeoTree or vim’s NerdTree, but largely inspired by the Project Explorer in Eclipse.
;; It shows the file system outlines of your projects in a simple tree layout allowing quick navigation and exploration, while also possessing basic file management utilities.
(load! "packages/ui/treemacs.el")

;; Languages
;; Java, Maven, Lombok setup
(load! "packages/lang/java.el")

;; JavaScript setup
(load! "packages/lang/javascript.el")

;; Mu4e mail setup
(load! "packages/email/mu4e.el")
