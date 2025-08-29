;;; module/config/tools-config/tools-editorconfig-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Global EditorConfig support for Emacs.
;; Automatically applies coding styles based on .editorconfig files.

;;; Code:

;; Enable editorconfig globally
(editorconfig-mode 1)

;; Optional: keybinding to show current EditorConfig settings
(map! :leader
      :desc "Show EditorConfig settings" "e c" #'editorconfig-show-current-settings)

(provide 'tools-editorconfig-config)

;;; tools-editorconfig-config.el ends here
