;;; module/config/ui-config/ui-doom-modeline-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Optimized Doom Modeline setup for Doom Emacs.
;; Displays buffer info, VCS, project, LSP, time, battery, and word count.

;;; Code:

(use-package! doom-modeline
  :defer t
  :init
  ;; Enable Doom Modeline globally
  (doom-modeline-mode 1)
  :config
  ;; Customize modeline appearance and features
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
        doom-modeline-enable-word-count t))

(provide 'ui-doom-modeline-config)

;;; ui-doom-modeline-config.el ends here
