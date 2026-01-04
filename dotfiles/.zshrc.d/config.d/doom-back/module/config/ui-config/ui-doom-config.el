;;; module/config/ui-config/ui-doom-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Global Doom Emacs configuration:
;; - User info, theme, modeline, and visual tweaks
;; - Indentation, scrolling, navigation, and performance optimizations
;; - Git, Copilot, and editing utilities

;;; Code:

;; ----------------------------------------
;; User Info
;; ----------------------------------------
(setq user-full-name "Borislav Dostumski"
      user-mail-address "b.dostumski@gmail.com")

;; ----------------------------------------
;; Theme and Visual Tweaks
;; ----------------------------------------
(setq doom-theme 'doom-ayu-mirage)
(add-to-list 'default-frame-alist '(undecorated . t))

(use-package! solaire-mode
  :config
  (solaire-global-mode +1))

(custom-set-faces!
  '(hl-line :inherit region :extend t))

;; ----------------------------------------
;; Performance Tweaks
;; ----------------------------------------
(setq gc-cons-threshold (* 256 1024 1024)        ;; 256MB (increased)
      read-process-output-max (* 8 1024 1024)    ;; 8MB (doubled)
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      fast-but-imprecise-scrolling t             ;; Added for smoother scrolling
      redisplay-skip-fontification-on-input t)   ;; Prevent fontification freezes

;; Garbage Collector Magic Hack
(use-package! gcmh
  :init (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 512 1024 1024))) ;; 512MB

;; ----------------------------------------
;; Project & System
;; ----------------------------------------
(setq projectile-auto-discover t
      use-package-always-ensure t)

;; ----------------------------------------
;; Line Numbers & Autosave
;; ----------------------------------------
(setq display-line-numbers-type 'visual
      auto-save-default t
      make-backup-files nil
      auto-save-interval 200
      auto-save-timeout 20)

;; ----------------------------------------
;; Editing Utilities
;; ----------------------------------------
(use-package! expand-region
  :bind ("C-=" . er/expand-region))

(use-package! visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(use-package! sudo-edit
  :commands sudo-edit)

;; ----------------------------------------
;; Path Setup - PERFORMANCE OPTIMIZED
;; ----------------------------------------
(use-package! exec-path-from-shell
  :defer t  ;; Don't load until needed
  :init
  ;; These settings significantly reduce startup time
  (setq exec-path-from-shell-check-startup-files nil  ;; Don't check startup files
        exec-path-from-shell-arguments '("-l")        ;; Use minimal shell arguments
        exec-path-from-shell-variables '("PATH")      ;; Only sync PATH variable
        exec-path-from-shell-warn-duration-millis 10000) ;; Only warn if >10 seconds
  :config
  ;; Only initialize when actually needed
  (defun my/setup-path-when-needed ()
    (unless (or (daemonp) (not (display-graphic-p)))
      (exec-path-from-shell-initialize)))
  ;; Run path setup after everything else has loaded
  (add-hook 'emacs-startup-hook #'my/setup-path-when-needed 90))

;; ----------------------------------------
;; Indentation Guides
;; ----------------------------------------
(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :defer t
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'stack
        highlight-indent-guides-auto-enabled t
        highlight-indent-guides-character ?•
        highlight-indent-guides-delay 0.5  ;; Add slight delay for performance
        highlight-indent-guides-highlight-current-column t)
  (custom-set-faces
   '(highlight-indent-guides-odd-face               ((t (:foreground "#3a4454"))))
   '(highlight-indent-guides-even-face              ((t (:foreground "#465163"))))
   '(highlight-indent-guides-character-face         ((t (:foreground "#3a4454"))))
   '(highlight-indent-guides-current-character-face ((t (:foreground "#A3BE8C"))))))

;; ----------------------------------------
;; Git Integration
;; ----------------------------------------
(use-package! git-link :commands git-link)
(use-package! git-messenger :commands git-messenger:popup-message)

;; ----------------------------------------
;; GitHub Copilot
;; ----------------------------------------
(after! copilot
  (add-hook 'prog-mode-hook #'copilot-mode)
  (setq copilot-idle-delay 0.5))

;; ----------------------------------------
;; Save State & Info
;; ----------------------------------------
(use-package! saveplace :hook (after-init . save-place-mode))
(use-package! savehist  :hook (after-init . savehist-mode))
(use-package! time
  :defer t
  :hook (after-init . display-time-mode)
  :custom
  (display-time-default-load-average nil)
  (display-time-mail-check-directory nil))
(use-package! battery :defer t :hook (after-init . display-battery-mode))

;; ----------------------------------------
;; Smooth Scrolling
;; ----------------------------------------
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local scroll-margin 10
                        scroll-conservatively 101
                        scroll-step 1
                        mouse-wheel-progressive-speed nil
                        mouse-wheel-follow-mouse t
                        scroll-preserve-screen-position t
                        mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control))))))

;; ----------------------------------------
;; Navigation
;; ----------------------------------------
(use-package! ace-window :bind ("M-o" . ace-window))
(use-package! avy        :bind ("C-:" . avy-goto-char))
(use-package! deadgrep   :commands deadgrep)
(use-package! wgrep      :commands wgrep-change-to-wgrep-mode)

;; ----------------------------------------
;; System Processes
;; ----------------------------------------
(use-package! proced
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t))

;; ----------------------------------------
;; Global Indentation
;; ----------------------------------------
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2)
(electric-indent-mode +1)

;; Sync with LSP/company
(setq-default lsp-inhibit-indent t)

(provide 'ui-doom-config)

;;; ui-doom-config.el ends here
