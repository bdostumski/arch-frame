;;; module/config/lang-config/lang-php-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; PHP configuration for Doom Emacs.
;; Provides LSP support, coding style settings, and leader keybindings
;; for running scripts, formatting, navigation, and testing.

;;; Code:

(after! php-mode
  ;; Enable LSP in PHP buffers
  (add-hook 'php-mode-hook #'lsp)

  ;; Optional: indentation style
  (setq php-mode-coding-style 'psr2
        c-basic-offset 4))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("p" . "php")
                                 :desc "Run PHP script" "r" #'php-execute-file
                                 :desc "Format buffer" "f" #'lsp-format-buffer
                                 :desc "Go to definition" "d" #'lsp-find-definition
                                 :desc "Run tests" "t" #'phpunit))))

(provide 'lang-php-config)

;;; lang-php-config.el ends here
