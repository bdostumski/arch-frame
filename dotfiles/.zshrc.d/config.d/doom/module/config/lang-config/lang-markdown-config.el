;;; lang-markdown-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Markdown configuration for Doom Emacs.
;; Provides:
;;  - Pandoc support for export
;;  - Leader keybindings for live preview and exporting

;;; Code:

(after! markdown-mode
  ;; Set Pandoc as the markdown command
  (setq markdown-command "pandoc")
  
  ;; Check if pandoc is installed
  (unless (executable-find "pandoc")
    (message "Warning: pandoc not found in PATH. Markdown export features may not work correctly.")))

;; Define a function to export to HTML and then convert to PDF
(defun markdown-export-to-html-and-pdf ()
  "Export markdown to HTML, and then use pandoc to convert HTML to PDF."
  (interactive)
  (let ((html-file (markdown-export-to-html))
        (pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
    (when html-file
      (if (executable-find "pandoc")
          (progn
            (shell-command (format "pandoc -f html -t pdf -o %s %s" 
                                   (shell-quote-argument pdf-file)
                                   (shell-quote-argument html-file)))
            (message "Exported to %s" pdf-file))
        (message "Cannot export to PDF: pandoc not found")))))

(provide 'lang-markdown-config)

;;; lang-markdown-config.el ends here
