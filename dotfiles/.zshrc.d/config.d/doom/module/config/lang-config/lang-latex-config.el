;;; module/config/lang-config/lang-latex-config.el -*- lexical-binding: t; -*-
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
  :defer t
  :after latex
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t
        auctex-latexmk-user-command "latexmk -pdf -interaction=nonstopmode %s"))

;; ----------------------------
;; Math preview
;; ----------------------------
(use-package! math-preview
  :defer t
  :hook (LaTeX-mode . math-preview-mode))

;; ----------------------------
;; CDLaTeX: fast math and environment insertion
;; ----------------------------
(use-package! cdlatex
  :defer t
  :hook (LaTeX-mode . turn-on-cdlatex))

;; ----------------------------
;; Core LaTeX / AUCTeX settings
;; ----------------------------
(after! latex
  ;; Enable LSP in LaTeX buffers
  (add-hook 'latex-mode-hook #'lsp)

  ;; Compilation and viewing settings
  (setq TeX-command-default "LatexMk"
        TeX-save-query nil
        TeX-PDF-mode t))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("l" . "latex")
                                 :desc "Compile LaTeX"    "c" #'TeX-command-master
                                 :desc "View PDF"         "v" #'TeX-view
                                 :desc "Clean aux files"  "x" #'TeX-clean))))

(provide 'lang-latex-config)

;;; lang-latex-config.el ends here
