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

(use-package! flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode))

(use-package! expand-region
  :bind ("C-=" . er/expand-region))

(use-package! visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(use-package! beacon
  :config
  (beacon-mode 1))

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
;; Snippet Configuration
;; ----------------------------------------

(use-package! yasnippet
  :defer t
  :commands yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package! yasnippet-snippets
  :after yasnippet)

;;; editor-config.el ends here
