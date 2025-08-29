;;; module/config-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module loads the default configuration settings that apply
;; globally across your Emacs setup. It acts as a foundation layer
;; on top of which other modules build.

;;; Code:

;; Default global configurations (keybindings, behaviors, misc tweaks)
(load! "config/default-config/config-default-config.el")

(provide 'config-module)

;;; config-module.el ends here
