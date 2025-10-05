;;; module/config/lang-config/lang-markdown-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Markdown configuration for Doom Emacs.
;; Provides:
;;  - Pandoc support for export
;;  - Leader keybindings for live preview and exporting

;;; Code:

(after! markdown-mode
  ;; Set Pandoc as the markdown command
  (setq markdown-command "pandoc"))

;; Leader keybindings
;;(map! :leader
;;      (:prefix-map ("m" . "markdown")
;;       :desc "Preview file" "p" #'markdown-live-preview-mode
;;       :desc "Export to PDF" "f" #'markdown-export-to-pdf
;;       :desc "Export to HTML" "h" #'markdown-export-to-html))

(provide 'lang-markdown-config)

;;; lang-markdown-config.el ends here
