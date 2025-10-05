;;; module/config/emacs-config/emacs-vc-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Version Control integration for Emacs.
;; Provides auto-refresh of VC status, Git/Mercurial support, and useful keybindings.

;;; Code:

;; Auto-refresh VC status in buffers
(global-auto-revert-mode 1)

;; Configure VC
(after! vc
  ;; Automatically follow symlinks in repositories
  (setq vc-follow-symlinks t)

  ;; Only handle Git and Mercurial
  (setq vc-handled-backends '(Git Hg))

  ;; Show diff directly in the buffer
  (setq vc-diff-auto-show t)

  ;; Make VC diff more readable
  (setq vc-diff-switches '("--patch" "--histogram"))

  ;; Keep VC state up to date
  (setq auto-revert-check-vc-info t)

  ;; Don't ask confirmation for reverting unchanged files
  (setq vc-suppress-confirm nil))

(use-package! diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Magit integration
(after! magit
  ;; Set default commit message max line length
  (setq git-commit-summary-max-length 50)
  (setq git-commit-fill-column 72)

  ;; Show word-granularity differences within regions
  (setq magit-diff-refine-hunk 'all)

  ;; Automatically clean up merged branches
  (setq magit-remove-stale-refs nil)

  ;; Show recent commits as a graph
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256")))

;; Optional: Forge for GitHub/GitLab integration
(use-package! forge
  :after magit
  :config
  (setq forge-topic-list-limit '(60 . 0)))

(provide 'emacs-vc-config)

;;; emacs-vc-config.el ends here
