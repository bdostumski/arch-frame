;;; module/config/tools-config/tools-rgb-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; RGB configuration for Doom Emacs.
;; Enables color preview and adjustment in programming buffers.
;; Provides leader keybindings for increasing, decreasing, and toggling color formats.

;;; Code:

;; ----------------------------
;; Enable rgb-mode globally
;; ----------------------------
(add-hook 'prog-mode-hook #'rgb-mode)

;; Optional: configure color step
(setq rgb-colorscale-step 10) ;; increment/decrement step for color adjustments

;; ----------------------------
;; Leader keybindings for color adjustments
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("c" . "color")
;;       :desc "Increase color" "+" #'rgb-increase-color
;;       :desc "Decrease color" "-" #'rgb-decrease-color
;;       :desc "Toggle color format" "f" #'rgb-toggle-format))

(provide 'tools-rgb-config)

;;; tools-rgb-config.el ends here
