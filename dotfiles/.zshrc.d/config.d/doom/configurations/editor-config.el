;;; configurations/editor-config.el --- Editor Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Editor behavior and snippet configuration.

;;; Code:


;; ----------------------------------------
;; Core Settings
;; ----------------------------------------

(setq projectile-auto-discover t)

;; NOTE: Doom manages packages declaratively, so this may be unnecessary:
(setq use-package-always-ensure t)

(setq display-line-numbers-mode 1)
(setq display-line-numbers-type 'visual)

(setq auto-save-default t)
(setq make-backup-files nil)
(setq auto-save-interval 200)
(setq auto-save-timeout 20)


;; ----------------------------------------
;; Snippet Configuration
;; ----------------------------------------

(use-package! yasnippet
  :defer t
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package! yasnippet-snippets
  :after yasnippet)

;; ----------------------------------------
;; Editor Enhancements
;; ----------------------------------------

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(after! rainbow-delimiters
  (custom-set-faces!
    '(rainbow-delimiters-depth-1-face :foreground "#F07178")
    '(rainbow-delimiters-depth-2-face :foreground "#FFCB6B")
    '(rainbow-delimiters-depth-3-face :foreground "#C3E88D")
    '(rainbow-delimiters-depth-4-face :foreground "#82AAFF")
    '(rainbow-delimiters-depth-5-face :foreground "#C792EA")
    '(rainbow-delimiters-depth-6-face :foreground "#89DDFF")
    '(rainbow-delimiters-depth-7-face :foreground "#F78C6C")
    '(rainbow-delimiters-depth-8-face :foreground "#FF5370")
    '(rainbow-delimiters-depth-9-face :foreground "#B0BEC5")
    '(show-paren-match-expression :background "#3B3B3B" :foreground "#FFD700" :weight bold)))
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Flycheck diagnostics in posframe tooltips
(use-package! flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-position 'window-bottom-left-corner
        flycheck-posframe-border-width 1))
(after! flycheck-posframe
  (custom-set-faces!
    '(flycheck-posframe-face :foreground "#ECEFF4")
    '(flycheck-posframe-border-face :background "#4B5263")))
(after! flycheck-posframe
  (custom-set-faces!
    '(flycheck-posframe-face :foreground "white")
    '(flycheck-posframe-border-face :background "gray")))
(setq flycheck-posframe-position 'point) ;; or 'window-bottom-left-corner
(setq flycheck-posframe-border-width 1)

(use-package! expand-region
  :bind ("C-=" . er/expand-region))

(use-package! visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(use-package! sudo-edit
  :commands sudo-edit)

(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package! solaire-mode
  :config
  (solaire-global-mode +1))

;; ----------------------------------------
;; Cursor Configuration
;; ----------------------------------------
(custom-set-faces!
  '(hl-line :inherit region :extend t))

;;; editor-config.el ends here
