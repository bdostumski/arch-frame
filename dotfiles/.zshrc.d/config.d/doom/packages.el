;;; $DOOMDIR/packages. el -*- lexical-binding:  t; -*-

;; ============================
;; Core Tools & Utilities
;; ============================
(package! sudo-edit)
(package! tldr)          
(package! pdf-tools)

;; ============================
;; Org & Notes
;; ============================
(package! org-appear)
(package! org-modern)
(package! org-download)

;; ============================
;; Optional Enhancements
;; ============================
;; (package! catppuccin-theme)  ; Popular modern theme
;; (package! rainbow-mode)      ; Colorize color codes

;; ============================
;; Java Development Enhancements
;; ============================
(package! lsp-java)           ; Should be included, but ensure it
(package! lsp-treemacs)       ; Tree views for symbols, references, etc. 
(package! dap-mode)           ; Debug Adapter Protocol
(package! gradle-mode)        ; Gradle task runner
(package! groovy-mode)        ; For build. gradle files

;;; packages.el ends here
