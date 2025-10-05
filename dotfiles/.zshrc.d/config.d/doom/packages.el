;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;;; Reordered packages by type

;; ============================
;; 🛠 Core Tools & Utilities
;; ============================

(package! beacon)                  ; briefly highlight cursor after jumps, improves visual tracking
(package! exec-path-from-shell)    ; import shell PATH into Emacs (important for GUI)
(package! git-link)                ; copy GitHub/GitLab file/line links to clipboard
(package! git-messenger)           ; show commit message at point (helpful for Git history)
(package! sudo-edit)               ; allows editing files with root permissions
(package! wgrep)                   ; edit grep/ack/rg results directly in buffer
(package! dirvish)                 ; modern, enhanced replacement for Dired
(package! ranger)                  ; Ranger-style file manager navigation
(package! rgb)                     ; color picker and RGB utilities
(package! evil-collection)         ; integrates Evil with many built-in and 3rd party modes
(package! evil-surround)           ; change/add/delete surrounding pairs ((), {}, "", etc.)
(package! evil-commentary)         ; Vim-style commenting
(package! evil-numbers)            ; increment/decrement numbers under cursor
(package! evil-exchange)           ; exchange text objects like in Vim
(package! evil-args)               ; Vim-style text object for function arguments
(package! evil-lion)               ; Vim-style alignment operator
(package! evil-matchit)            ; jump between matching tags/blocks
(package! evil-indent-plus)        ; enhanced indentation text objects
(package! evil-visualstar)         ; * search for visual selection
(package! vimish-fold)             ; Vim-style code folding
(package! evil-vimish-fold)        ; integrates Vimish fold with Evil
(package! fold-dwim)               ; "Do What I Mean" folding utilities
(package! fringe-helper)           ; helps draw custom fringes (used by other packages)
(package! aggressive-indent)       ; automatically reindents code as you type.
(package! origami)                 ; code folding framework for various languages
(package! undo-tree)               ; undo-tree
(package! eval-sexp-fu)

;; ============================
;; 🧠 Code Intelligence & LSP
;; ============================

(package! lsp-mode)                ; Language Server Protocol support
(package! lsp-ui)                  ; UI enhancements for LSP (sideline, doc popups)
(package! lsp-treemacs)            ; LSP symbols in Treemacs sidebar
(package! lsp-java)                ; Java LSP configuration
(package! dap-mode)                ; Debug Adapter Protocol (debugger integration)
(package! flycheck)                ; real-time syntax checking
(package! flycheck-posframe)       ; display Flycheck errors in a floating frame
(package! gradle-mode)             ; Gradle integration for Java projects
(package! maven-test-mode)         ; Maven integration for Java testing
(package! rainbow-delimiters)      ; color-coded parentheses for nested expressions
(package! plantuml-mode)           ; PlantUML support for diagrams

;; ============================
;; 🔤 Completion & Snippets
;; ============================

(package! company)                 ; autocompletion framework
(package! company-box)             ; fancy UI for company completions
(package! company-prescient)       ; sorts completion candidates intelligently
(package! company-quickhelp)       ; pop-up documentation for completions
(package! company-statistics)      ; learning-based sorting of company completions
(package! company-shell)           ; company completions for shell scripts
(package! company-math)            ; completions for LaTeX/math symbols
(package! company-web)             ; completions for HTML/CSS/JS
(package! emmet-mode)              ; Emmet expansion (HTML/CSS snippets)
(package! bash-completion)         ; Bash completion for shell commands
(package! ivy)                     ; completion and narrowing framework
(package! ivy-posframe)            ; display Ivy completions in a floating frame
(package! ivy-prescient)           ; sorts Ivy candidates intelligently
(package! ivy-rich)                ; richer Ivy completion UI with metadata
(package! all-the-icons-ivy-rich)  ; icon-enhanced Ivy/Helm interface
(package! jq-mode)                 ; JSON query editing with jq syntax highlighting
(package! prettier-js)             ; format JS/TS/HTML/CSS on save
(package! yasnippet-snippets)      ; library of ready-to-use code snippets
(package! fish-mode)               ; fish shell syntax highlighting

;; ============================
;; 🎨 UI Enhancements
;; ============================

(package! ace-window)              ; fast window switching
(package! all-the-icons)           ; icons for buffers, treemacs, mode-line
(package! all-the-icons-dired)     ; Dired icons support
(package! avy)                     ; jump to visible text using key sequences
(package! deadgrep)                ; ripgrep frontend for searching code/projects
(package! expand-region)           ; expand selection by semantic units
(package! highlight-indent-guides) ; visually indicate indentation levels
(package! highlight-parentheses)   ; highlight matching parentheses
(package! hl-todo)                 ; highlight TODO/FIXME comments
(package! minimap)                 ; code minimap like in modern IDEs
(package! smooth-scrolling)        ; smooth scroll experience
(package! solaire-mode)            ; highlight buffers differently from main UI
(package! treemacs)                ; project/file explorer sidebar
(package! treemacs-all-the-icons)  ; icon support for Treemacs
(package! treemacs-icons-dired)    ; Treemacs icons inside Dired buffers
(package! treemacs-magit)          ; integrate Treemacs with Magit
(package! treemacs-persp)          ; integrate Treemacs with perspectives/workspaces
(package! treemacs-projectile)     ; integrate Treemacs with Projectile projects
(package! treemacs-tab-bar)        ; tabs for multiple Treemacs workspaces
(package! tree-sitter)             ; fast syntax highlighting and parsing
(package! tree-sitter-langs         ; language definitions for tree-sitter
  :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs"))
(package! visual-fill-column)      ; wrap text visually for reading/editing

;; ============================
;; ✍️ Writing, Org & Notes
;; ============================

(package! deft)                    ; quick note-taking and searching
(package! emojify)                  ; display emojis in Emacs buffers
(package! org-appear)               ; automatically show/hide Org emphasis markers
(package! org-download)             ; drag & drop images into Org files
(package! org-gcal)                 ; sync Google Calendar with Org
(package! org-modern)               ; modern Org UI enhancements
(package! org-roam-ui)              ; Org-roam visual interface (graph, dashboards)
(package! org-super-agenda)         ; enhance Org agenda views
(package! ox-hugo)                  ; export Org content to Hugo (static site generator)
(package! auctex-latexmk)           ; compile LaTeX projects automatically
(package! math-preview)             ; preview LaTeX/math in Emacs
(package! cdlatex)                  ; quick LaTeX math input

;; ============================
;; 🧪 Testing & Dev Tools
;; ============================

(package! cider)                    ; Clojure REPL integration
(package! clojure-mode)             ; Clojure syntax highlighting & editing
(package! ejc-sql)                  ; SQL database client in Emacs
(package! sql)                      ; Emacs built-in SQL support
(package! graphql-mode)             ; GraphQL syntax support
(package! test-simple)              ; simple testing framework for Emacs

;; ============================
;; 🤖 AI / Copilot / LLM
;; ============================

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el")) ; GitHub Copilot integration

;; ============================
;; 🧩 Optional / Disabled
;; ============================

;; (package! ligature)              ; enable font ligatures
;; (package! neotree)               ; alternative to Treemacs
;; Vertico stack (if you want to switch from Ivy)
;; (package! vertico)
;; (package! orderless)
;; (package! marginalia)
;; (package! consult)
;; (package! embark)
;; (package! embark-consult)

;;; packages.el ends here
