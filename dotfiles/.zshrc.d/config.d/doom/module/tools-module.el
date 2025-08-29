;;; module/tools-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module loads configurations for development tools and integrations.
;; It covers collaboration, debugging, code evaluation, project management,
;; version control, build tools, containerization, testing, and more.

;;; Code:

;; Collaboration tools (e.g., Gist, Forge, etc.)
(load! "config/tools-config/tools-collab-config.el")

;; Debugging (DAP mode, GDB, etc.)
(load! "config/tools-config/tools-debugger-config.el")

;; Direnv integration (per-project environment variables)
(load! "config/tools-config/tools-direnv-config.el")

;; Docker integration (manage containers and images from Emacs)
(load! "config/tools-config/tools-docker-config.el")

;; EditorConfig support (consistent coding styles across projects)
(load! "config/tools-config/tools-editorconfig-config.el")

;; Eval tools (REPLs, code evaluation, inline results, etc.)
(load! "config/tools-config/tools-eval-config.el")

;; LLM/AI tools (integrations with AI coding assistants)
(load! "config/tools-config/tools-llm-config.el")

;; Lookup tools (documentation lookup, symbol search, etc.)
(load! "config/tools-config/tools-lookup-config.el")

;; Language Server Protocol (LSP) support
(load! "config/tools-config/tools-lsp-config.el")

;; Magit and Git integrations (best-in-class VCS interface)
(load! "config/tools-config/tools-magit-config.el")

(load! "config/tools-config/tools-git-config.el")

;; Makefile integration (run builds inside Emacs)
(load! "config/tools-config/tools-make-config.el")

;; Pass (password manager integration)
(load! "config/tools-config/tools-pass-config.el")

;; PDF tools (reading, annotating, syncing with LaTeX, etc.)
(load! "config/tools-config/tools-pdf-config.el")

;; Projectile (project management, navigation, search, etc.)
(load! "config/tools-config/tools-projectile-config.el")

;; RGB color picker (for UI/UX and CSS editing)
(load! "config/tools-config/tools-rgb-config.el")

;; Task runner (for npm, yarn, gulp, etc.)
(load! "config/tools-config/tools-taskrunner-config.el")

;; Testing framework integration (unit tests, runners, etc.)
(load! "config/tools-config/tools-testing-config.el")

;; Tmux integration (synchronize shells and panes with Emacs)
(load! "config/tools-config/tools-tmux-config.el")

;; Tree-sitter (advanced syntax parsing and highlighting)
(load! "config/tools-config/tools-tree-sitter-config.el")

;; Upload tools (FTP, SSH, SFTP file transfers)
(load! "config/tools-config/tools-upload-config.el")

(provide 'tools-module)

;;; tools-module.el ends here
