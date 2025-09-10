;;; module/config/lang-config/lang-java-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Java setup with LSP, formatting, DAP, and keybindings for compilation and testing.

;;; Code:

(after! java-mode
  ;; Enable LSP in Java buffers
  (add-hook 'java-mode-hook #'lsp)

  ;; Optional formatting style
  (setq c-basic-offset 4
        c-default-style "java"))

;; Leader keybindings for Java
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("j" . "java")
                                 :desc "Compile project" "c" #'compile
                                 :desc "Run debugger"   "d" #'dap-debug
                                 :desc "Run tests"      "t" #'lsp-java-run-test))))

(provide 'lang-java-config)

;;; lang-java-config.el ends here
