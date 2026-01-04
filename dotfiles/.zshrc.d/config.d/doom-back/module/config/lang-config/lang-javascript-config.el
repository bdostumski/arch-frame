;;; lang-javascript-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; JavaScript setup with LSP, formatting, and useful keybindings.

;;; Code:

;; Ensure necessary packages are installed
(use-package nodejs-repl)
(use-package prettier-js)

(after! js
  ;; Enable LSP in JavaScript buffers
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'prettier-js-mode)

  ;; Optional indentation settings
  (setq js-indent-level 2
        js-switch-indent-offset 2)

  ;; Set up eslint integration with LSP
  (setq lsp-eslint-enable t))

;; Modern JS support with js2-mode
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp)
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning nil))

;; TypeScript support
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))

(provide 'lang-javascript-config)

;;; lang-javascript-config.el ends here
