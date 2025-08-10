;;; configurations/ui-config.el --- UI Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; UI and visual enhancements for Doom Emacs.

;;; Code:

;; ----------------------------------------
;; User Info
;; ----------------------------------------

(setq user-full-name "Borislav Dostumski"
      user-mail-address "b.dostumski@gmail.com")

;; ----------------------------------------
;; Theme and Visual Tweaks
;; ----------------------------------------

(setq doom-theme 'doom-wilmersdorf)
(add-to-list 'default-frame-alist '(undecorated . t))
(solaire-global-mode +1)

(display-time-mode 1)

;; ----------------------------------------
;; Treemacs and Integrations
;; ----------------------------------------

(use-package! treemacs
  :defer t)

(use-package! treemacs-all-the-icons
  :after treemacs)

(use-package! treemacs-icons-dired
  :after treemacs)

(use-package! treemacs-magit
  :after (treemacs magit))

(use-package! treemacs-persp
  :after (treemacs persp-mode))

(use-package! treemacs-projectile
  :after (treemacs projectile))

(use-package! treemacs-tab-bar
  :after treemacs)

;; ----------------------------------------
;; Doom Modeline
;; ----------------------------------------

(use-package! doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-encoding t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-minor-modes nil
        doom-modeline-lsp t
        doom-modeline-enable-word-count t))

;; ----------------------------------------
;; Navigation
;; ----------------------------------------

(use-package! ace-window
  :bind ("M-o" . ace-window))

(use-package! avy
  :bind ("C-:" . avy-goto-char))

(use-package! deadgrep
  :commands deadgrep)

(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode)

;; ----------------------------------------
;; Note-taking with Deft
;; ----------------------------------------

(use-package! deft
  :config
  (setq deft-directory "~/notes"
        deft-extensions '("org" "md" "txt")
        deft-recursive t))

;; ----------------------------------------
;; Emoji Support
;; ----------------------------------------

(use-package! emojify
  :hook (after-init . global-emojify-mode))

;; ----------------------------------------
;; TODO Highlighting
;; ----------------------------------------

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode))

;; ----------------------------------------
;; Indentation Guides
;; ----------------------------------------

(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; ----------------------------------------
;; Minimap
;; ----------------------------------------

(use-package! minimap
  :commands minimap-mode)

;; ----------------------------------------
;; Smooth Scrolling
;; ----------------------------------------

(use-package! smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;;; ui-config.el ends here
