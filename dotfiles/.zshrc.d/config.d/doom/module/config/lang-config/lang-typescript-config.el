;;; module/config/lang-config/lang-typescript-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; TypeScript configuration for Doom Emacs.
;; Enables LSP, sets indentation, and provides leader keybindings for
;; formatting, navigation, and running tests.

;;; Code:

(after! typescript-mode
  ;; Enable LSP in TypeScript buffers
  (add-hook 'typescript-mode-hook #'lsp)

  ;; Formatting
  (setq typescript-indent-level 2))

;; ----------------------------
;; Leader keybindings for TypeScript
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("t" . "typescript")
                                 :desc "Format buffer"      "f" #'lsp-format-buffer
                                 :desc "Go to definition"   "d" #'lsp-find-definition
                                 :desc "Run tests"          "t" #'npm-test-current-project))))

(provide 'lang-typescript-config)

;;; lang-typescript-config.el ends here
