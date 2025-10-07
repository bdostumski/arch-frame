;;; lang-latex-config.el --- LaTeX configuration for Doom Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; LaTeX setup for Doom Emacs.
;; Features:
;;  - AUCTeX with latexmk compilation
;;  - Math preview for inline equations
;;  - CDLaTeX for fast math insertion
;;  - LSP support
;;  - Leader keybindings for compile, view, and clean

;;; Code:

;; ----------------------------
;; AUCTeX + latexmk
;; ----------------------------
(use-package! auctex-latexmk
  :after latex
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t
        auctex-latexmk-user-command "latexmk -pdf -interaction=nonstopmode %s"))

;; ----------------------------
;; Math preview
;; ----------------------------
(use-package! math-preview
  :hook (LaTeX-mode . math-preview-mode))

;; ----------------------------
;; CDLaTeX: fast math and environment insertion
;; ----------------------------
(use-package! cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex))

;; ----------------------------
;; Core LaTeX / AUCTeX settings
;; ----------------------------
(after! latex
  ;; Enable LSP in LaTeX buffers
  (add-hook 'LaTeX-mode-hook #'lsp) ;; Fixed: LaTeX-mode-hook instead of latex-mode-hook

  ;; Compilation and viewing settings
  (setq TeX-command-default "LatexMk"
        TeX-save-query nil
        TeX-PDF-mode t))

(provide 'lang-latex-config)
;;; lang-latex-config.el ends here
