;;; module/config/os-config/os-which-key-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Which-Key configuration for Doom Emacs.
;; Sets popup behavior, delays, and display limits.

;;; Code:

(after! which-key
  ;; Configure Which-Key popup
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.1
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-max-description-length 40))

(provide 'os-which-key-config)

;;; os-which-key-config.el ends here
