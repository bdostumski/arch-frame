;;; packages.el --- Doom Emacs Package Declarations -*- no-byte-compile: t; -*-

;; This file declares all packages used by your Doom config.
;; Run `doom sync` after modifying it, then restart Emacs.

;; ============================
;; 🛠 Core Tools & Utilities
;; ============================

(package! beacon)                  ; highlight cursor after jumps
(package! exec-path-from-shell)    ; inherit shell env vars in GUI Emacs
(package! git-link)                ; copy links to GitHub/GitLab commits/files
(package! git-messenger)           ; show commit message at point
(package! sudo-edit)               ; open files as root
(package! wgrep)                   ; edit grep results in-place
(package! dirvish)                 ; modern Dired interface

;; ============================
;; 🧠 Code Intelligence & LSP
;; ============================

(package! dap-mode)
(package! flycheck-posframe)       ; show Flycheck errors in posframe
(package! gradle-mode)
(package! lsp-java)
(package! lsp-mode)
(package! lsp-treemacs)
(package! lsp-ui)
(package! maven-test-mode)
(package! rainbow-delimiters)      ; rainbow parens for Lisp modes

;; ============================
;; 🔤 Completion & Snippets
;; ============================

(package! all-the-icons-ivy-rich)
(package! bash-completion)
(package! company)
(package! company-box)
(package! emmet-mode)
(package! fish-mode)
(package! ivy)
(package! ivy-posframe)
(package! ivy-prescient)
(package! ivy-rich)
(package! jq-mode)
(package! prettier-js)
(package! yasnippet-snippets)

;; ============================
;; 🎨 UI Enhancements
;; ============================

(package! ace-window)
(package! all-the-icons)
(package! all-the-icons-dired)
(package! avy)
(package! deadgrep)
(package! expand-region)
(package! highlight-indent-guides)
(package! hl-todo)
(package! minimap)
(package! smooth-scrolling)
(package! solaire-mode)
(package! treemacs)
(package! treemacs-all-the-icons)
(package! treemacs-icons-dired)
(package! treemacs-magit)
(package! treemacs-persp)
(package! treemacs-projectile)
(package! treemacs-tab-bar)
(package! tree-sitter)
(package! tree-sitter-langs
  :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs"))
(package! visual-fill-column)

;; ============================
;; ✍️ Writing, Org & Notes
;; ============================

(package! deft)
(package! emojify)
(package! org-appear)
(package! org-download)
(package! org-gcal)
(package! org-modern)
(package! org-roam-ui)
(package! org-super-agenda)
(package! ox-hugo)
(package! auctex-latexmk)
(package! math-preview)
(package! cdlatex)

;; ============================
;; 🧪 Testing & Dev Tools
;; ============================

(package! cider)
(package! clojure-mode)
(package! ejc-sql)
(package! graphql-mode)
(package! test-simple)

;; ============================
;; 🤖 AI / Copilot / LLM
;; ============================

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el"))

;; ============================
;; 🧩 Optional / Disabled
;; ============================

;; (package! ligature)              ; ligatures support
;; (package! neotree)               ; alternative to treemacs

;; Vertico stack (if you want to switch from Ivy)
;; (package! vertico)
;; (package! orderless)
;; (package! marginalia)
;; (package! consult)
;; (package! embark)
;; (package! embark-consult)

;;; packages.el ends here
