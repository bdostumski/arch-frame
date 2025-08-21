;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

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
(package! dirvish)           ;; Modern Dired replacement
(package! ranger)            ;; Ranger-style file navigation

;; ============================
;; 🧠 Code Intelligence & LSP
;; ============================

(package! dap-mode)
(package! flycheck)
(package! flycheck-posframe)       ; show Flycheck errors in posframe
(package! gradle-mode)
(package! lsp-java)
(package! lsp-mode)
(package! lsp-treemacs)
(package! lsp-ui)
(package! maven-test-mode)
(package! rainbow-delimiters)      ; rainbow parens for Lisp modes
(package! plantuml-mode)

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
