;;; module/config/lang-config/lang-web-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Web development configuration for Doom Emacs.
;; Enables LSP in web-mode, sets indentation, and provides leader keybindings
;; for formatting, navigation, and running the project.

;;; Code:

(after! web-mode
  ;; Enable LSP in web-mode buffers
  (add-hook 'web-mode-hook #'lsp)

  ;; Indentation settings
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; ----------------------------
;; Leader keybindings for Web
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("w" . "web")
                                 :desc "Format buffer"      "f" #'lsp-format-buffer
                                 :desc "Go to definition"   "d" #'lsp-find-definition
                                 :desc "Run project"        "r" #'npm-run-current-project))))

(provide 'lang-web-config)

;;; lang-web-config.el ends here
