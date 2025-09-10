;;; module/config/lang-config/lang-cc-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ configuration with LSP, formatting, and convenient keybindings.

;;; Code:

(after! cc-mode
  ;; Enable LSP for C/C++
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)

  ;; Style and indentation
  (setq c-default-style "linux"
        c-basic-offset 4))

;; Leader keybindings for C/C++
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("c" . "C/C++")
                                 :desc "Compile project" "c" #'compile
                                 :desc "Run debugger" "d" #'gdb
                                 :desc "Switch header/source" "s" #'ff-find-other-file))))

(provide 'lang-cc-config)

;;; lang-cc-config.el ends here
