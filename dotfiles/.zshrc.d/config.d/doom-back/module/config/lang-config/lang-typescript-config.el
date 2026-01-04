;;; lang-web-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Web development configuration for Doom Emacs.
;; Enables LSP in web-mode, sets indentation, and provides leader keybindings
;; for formatting, navigation, and running the project.

;;; Code:

;; Ensure required packages
(use-package! web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx?\\'" "\\.tsx?\\'"))

;; Define npm run helper function
(defun npm-run-current-project (script)
  "Run npm SCRIPT in the current project."
  (interactive "sScript name: ")
  (let ((default-directory (doom-project-root)))
    (compile (format "npm run %s" script))))

(after! web-mode
  ;; Enable LSP in web-mode buffers, with error handling
  (add-hook! 'web-mode-hook
    (defun +web-setup-lsp-h ()
      (if (fboundp 'lsp)
          (lsp)
        (message "LSP not available. Install lsp-mode package."))))

  ;; Indentation settings
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t))

;; LSP server configurations
(after! lsp-mode
  (when (executable-find "typescript-language-server")
    (add-to-list 'lsp-disabled-clients '(web-mode . (angular-ls))))
  
  ;; Configure specific servers
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-verbosity=off")
        lsp-html-validate-scripts t
        lsp-css-validate t))

(provide 'lang-web-config)

;;; lang-web-config.el ends here
