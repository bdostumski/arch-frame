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

;; LSP Configuration for C/C++ (prefer clangd)
(after! lsp-mode
  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=iwyu"
                                  "--header-insertion-decorators=0"))
  (setq lsp-clangd-binary-path (executable-find "clangd"))
  (setq lsp-clients-clangd-executable (executable-find "clangd")))

;; Formatting with clang-format
(after! format
  (set-formatter! 'clang-format
    '("clang-format"
      ("-assume-filename=%S" (or buffer-file-name mode-result "")))
    :modes '(c-mode c++-mode)))

;; Customize Flycheck for C/C++
(after! flycheck
  (setq flycheck-clang-language-standard "c++17"
        flycheck-gcc-language-standard "c++17"))

;; Additional completion settings
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  (add-hook 'c-mode-hook (lambda () (setq company-backends '(company-capf))))
  (add-hook 'c++-mode-hook (lambda () (setq company-backends '(company-capf)))))

(provide 'lang-cc-config)

;;; lang-cc-config.el ends here
