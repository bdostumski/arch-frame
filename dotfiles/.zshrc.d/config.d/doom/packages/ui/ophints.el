;; Enable (evil-goggles-mode), then edit text like you normally would, try for example yy, p, dd in normal state.

(use-package! evil-goggles
  :after evil
  :config
  (setq evil-goggles-enable-paste t
        evil-goggles-enable-delete t
        evil-goggles-enable-change t
        evil-goggles-enable-yank t
        evil-goggles-enable-join t
        evil-goggles-enable-fill-and-move t
        evil-goggles-pulse t)

  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))
