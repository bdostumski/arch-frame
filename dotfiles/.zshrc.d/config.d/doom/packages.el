;;; $DOOMDIR/packages. el -*- lexical-binding:  t; -*-

;; ============================
;; Core Tools & Utilities
;; ============================

(package! sudo-edit)
(package! exec-path-from-shell)
(package! tldr)          
(package! pdf-tools)

;; Web Tools
(package! simple-httpd)
(package! htmlize)
(package! websocket
  :recipe (:host github :repo "ahyatt/emacs-websocket")) ;; Dependency for org-mode

;; AI Tools
(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))

;; ============================
;; Database
;; ============================
(package! ejc-sql) ;; Database connection
(package! emacsql) ;; Required dependency for ejc-sql
(package! dash-functional) 

;; ============================
;; Org & Notes
;; ============================
(package! org-appear)
(package! org-modern)
(package! org-download)
(package! org-superstar)
(package! org-roam-ui)
(package! ob-restclient)     
(package! restclient)        

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
