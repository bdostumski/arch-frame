;;; module/tools-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module loads configurations for development and productivity tools in Emacs.
;; It covers project management, collaboration, debugging, LSP/AI, version control,
;; containerization, testing, and other utilities.
;;
;; Safety/load order rationale:
;; 1. Lightweight or foundational integrations are loaded first.
;; 2. Tools that depend on external processes or other packages (LSP, DAP, Docker) are loaded after.
;; 3. Optional or heavy integrations (PDF tools, RGB picker) are loaded last.
;; 4. Load order ensures dependencies are initialized before consumers (e.g., Git before Magit, Projectile before LSP).

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Collaboration tools
;; ---------------------------------------------------------------------------
;; Gist, Forge, and other collaboration utilities. Low-risk, foundational.
(load! "config/tools-config/tools-collab-config.el")

;; ---------------------------------------------------------------------------
;; 2. Version control
;; ---------------------------------------------------------------------------
;; Git backend and Magit interface loaded early so other tools can rely on VCS info.
(load! "config/tools-config/tools-git-config.el")
(load! "config/tools-config/tools-magit-config.el")

;; ---------------------------------------------------------------------------
;; 3. Project management & environment
;; ---------------------------------------------------------------------------
;; Projectile for project navigation
;; Direnv for per-project environment variables
;; EditorConfig for consistent coding style
(load! "config/tools-config/tools-projectile-config.el")
(load! "config/tools-config/tools-direnv-config.el")
(load! "config/tools-config/tools-editorconfig-config.el")

;; ---------------------------------------------------------------------------
;; 4. Language Server Protocol & evaluation
;; ---------------------------------------------------------------------------
;; LSP integration and evaluation/repl tools
(load! "config/tools-config/tools-lsp-config.el")
(load! "config/tools-config/tools-eval-config.el")

;; ---------------------------------------------------------------------------
;; 5. Debugging
;; ---------------------------------------------------------------------------
;; DAP mode, GDB, and other debugger configurations
(load! "config/tools-config/tools-debugger-config.el")

;; ---------------------------------------------------------------------------
;; 6. Containerization
;; ---------------------------------------------------------------------------
;; Docker integration for managing containers/images
(load! "config/tools-config/tools-docker-config.el")

;; ---------------------------------------------------------------------------
;; 7. AI / LLM tools
;; ---------------------------------------------------------------------------
;; Integration with AI coding assistants (Codeium, Copilot, etc.)
(load! "config/tools-config/tools-llm-config.el")

;; ---------------------------------------------------------------------------
;; 8. Lookup / documentation tools
;; ---------------------------------------------------------------------------
(load! "config/tools-config/tools-lookup-config.el")

;; ---------------------------------------------------------------------------
;; 9. Build & automation
;; ---------------------------------------------------------------------------
;; Make, task runners (npm, yarn, gulp, etc.)
(load! "config/tools-config/tools-make-config.el")
(load! "config/tools-config/tools-taskrunner-config.el")

;; ---------------------------------------------------------------------------
;; 10. Password management
;; ---------------------------------------------------------------------------
;; Pass or other password manager integration
(load! "config/tools-config/tools-pass-config.el")

;; ---------------------------------------------------------------------------
;; 11. PDF tools
;; ---------------------------------------------------------------------------
;; PDF reading, annotating, and syncing (LaTeX, org-pdf)
(load! "config/tools-config/tools-pdf-config.el")

;; ---------------------------------------------------------------------------
;; 12. Syntax & highlighting enhancements
;; ---------------------------------------------------------------------------
;; Tree-sitter advanced parsing and highlighting
(load! "config/tools-config/tools-tree-sitter-config.el")

;; ---------------------------------------------------------------------------
;; 13. Terminal & synchronization
;; ---------------------------------------------------------------------------
;; Tmux integration
(load! "config/tools-config/tools-tmux-config.el")

;; ---------------------------------------------------------------------------
;; 14. File transfer & upload
;; ---------------------------------------------------------------------------
;; FTP, SFTP, SSH upload tools
(load! "config/tools-config/tools-upload-config.el")

;; ---------------------------------------------------------------------------
;; 15. Optional / heavy integrations
;; ---------------------------------------------------------------------------
;; RGB color picker (for UI/CSS editing), loaded last
(load! "config/tools-config/tools-rgb-config.el")

(provide 'tools-module)

;;; tools-module.el ends here
