;;; module/config/ui-config/ui-minimap-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Display a minimap of the current buffer for quick code navigation.

;;; Code:

(use-package! minimap
  :defer t
  :commands (minimap-mode minimap-create)
  :init
  :config
  ;; Minimap settings
  (setq minimap-window-location 'right         ;; position: right or left
        minimap-width-fraction 0.15           ;; fraction of frame width
        minimap-update-delay 0.1               ;; update delay for performance
        minimap-hide-fringes t
        minimap-recenter-type 'middle          ;; recenter behavior
        minimap-dedicated-window t             ;; separate window
        minimap-minimum-width 20
        minimap-major-modes t                  ;; show in all major modes
        minimap-buffer-name "*minimap*"))

(provide 'ui-minimap-config)

;;; ui-minimap-config.el ends here
