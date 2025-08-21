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

;;(setq doom-theme 'doom-wilmersdorf)
(setq doom-theme 'doom-ayu-mirage)
(add-to-list 'default-frame-alist '(undecorated . t))
(solaire-global-mode +1)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :custom
  (display-time-default-load-average nil)
  (display-time-mail-check-directory nil))

(use-package battery
  :ensure nil
  :hook (after-init . display-battery-mode))

;; ----------------------------------------
;; System prcesses
;; ---------------------------------------
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t))

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
  (setq doom-modeline-height 20
        doom-modeline-bar-width 4
        doom-modeline-buffer-file-name t
        doom-modeline-buffer-state-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode t
        doom-modeline-minor-modes nil
        doom-modeline-vcs-max-length 15
        doom-modeline-github nil
        doom-modeline-time t
        doom-modeline-battery t
        doom-modeline-project-detection 'project
        doom-modeline-project-name t
        doom-modeline-format 'main
        doom-modeline-buffer-encoding t
        doom-modeline-lsp t
        doom-modeline-enable-word-count t
        ))

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
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#FF6347")
          ("FIXME" . "#FFFF00")
          ("BUG" . "#FF4500")
          ("HACK" . "#8A2BE2"))))

;; ----------------------------------------
;; Indentation Guides
;; ----------------------------------------
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'stack
      highlight-indent-guides-auto-enabled t
      highlight-indent-guides-character ?•
      ;;highlight-indent-guides-character ?▶
      highlight-indent-guides-auto-odd-face-perc 5
      highlight-indent-guides-auto-even-face-perc 10
      highlight-indent-guides-auto-character-face-perc 15
      highlight-indent-guides-highlight-current-column t)
(custom-set-faces
 '(highlight-indent-guides-odd-face  ((t (:foreground "#3a4454"))))
 '(highlight-indent-guides-even-face ((t (:foreground "#465163"))))
 '(highlight-indent-guides-character-face ((t (:foreground "#3a4454"))))
 '(highlight-indent-guides-current-character-face ((t (:foreground "#A3BE8C")))))

;; ----------------------------------------
;; Smooth Scrolling
;; ----------------------------------------

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local scroll-margin 10
                        scroll-conservatively 101
                        scroll-step 1
                        mouse-wheel-progressive-speed nil
                        mouse-wheel-follow-mouse nil
                        scroll-preserve-screen-position nil
                        mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control))))))

;;; ui-config.el ends here
