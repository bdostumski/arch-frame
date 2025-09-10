;;; module/config/lang-config/lang-javascript-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; JavaScript setup with LSP, formatting, and useful keybindings.

;;; Code:

(after! js
  ;; Enable LSP in JavaScript buffers
  (add-hook 'js-mode-hook #'lsp)

  ;; Optional indentation settings
  (setq js-indent-level 2
        js-switch-indent-offset 2))

;; Leader keybindings for JavaScript
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("a" . "javascript")
                                 :desc "Run current file" "r" #'nodejs-repl
                                 :desc "Format buffer"     "f" #'lsp-format-buffer
                                 :desc "Go to definition"  "d" #'lsp-find-definition
                                 :desc "Run tests"         "t" #'npm-test-current-project))))

(provide 'lang-javascript-config)

;;; lang-javascript-config.el ends here
