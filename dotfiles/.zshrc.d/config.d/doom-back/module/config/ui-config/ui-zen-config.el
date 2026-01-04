;;; module/config/ui-config/ui-zen-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Zen Mode configuration for distraction-free writing and coding.
;; Integrates writeroom-mode with visual-fill-column for centered text,
;; optional line-number hiding, and fringe adjustments.
;; Includes Vim-style toggle keybinding.

;;; Code:

;; ----------------------------------------
;; Writeroom (Zen) Mode settings
;; ----------------------------------------
(after! writeroom-mode
  ;; Set dynamic width relative to frame
  (setq writeroom-width (/ (frame-width) 1.5)
        writeroom-mode-line t
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(org-mode markdown-mode prog-mode))

  ;; Hide line numbers and fringe in Zen mode, restore on exit
  (add-hook 'writeroom-mode-hook
            (lambda ()
              (if writeroom-mode
                  (progn
                    (display-line-numbers-mode -1)
                    (fringe-mode 0))
                (progn
                  (display-line-numbers-mode 1)
                  (fringe-mode nil))))))

;; ----------------------------------------
;; Visual-fill-column integration
;; ----------------------------------------
(use-package! visual-fill-column
  :after writeroom-mode
  :hook (writeroom-mode . visual-fill-column-mode)
  :config
  ;; Center text dynamically and match writeroom width
  (setq visual-fill-column-center-text t
        visual-fill-column-width (/ (frame-width) 1.5)))

(provide 'ui-zen-config)

;;; ui-zen-config.el ends here
