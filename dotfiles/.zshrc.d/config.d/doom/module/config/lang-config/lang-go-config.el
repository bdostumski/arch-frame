;;; module/config/lang-config/lang-go-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Go development configuration with LSP, auto-formatting, and leader keybindings.

;;; Code:

(after! go-mode
  ;; Enable LSP in Go buffers
  (add-hook 'go-mode-hook #'lsp)
  ;; Format with gofmt on save
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Leader keybindings for Go
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("g" . "go")
                                 :desc "Run go build"          "b" #'compile
                                 :desc "Run go test"           "t" #'go-test-current-project
                                 :desc "Go to definition"      "d" #'godef-jump))))

(provide 'lang-go-config)

;;; lang-go-config.el ends here
