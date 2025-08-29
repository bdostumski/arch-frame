;;; module/lang-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module centralizes configuration for all programming and markup languages.
;; Each language has its own dedicated config file inside `config/lang-config/`,
;; which keeps things modular, organized, and easy to extend or disable.

;;; Code:

;; C / C++ language support
(load! "config/lang-config/lang-cc-config.el")

;; Common Lisp support
(load! "config/lang-config/lang-common-lisp-config.el")

;; Data formats (CSV, XML, etc.)
(load! "config/lang-config/lang-data-config.el")

;; Emacs Lisp (with LSP integration)
(load! "config/lang-config/lang-emacs-lisp-config.el")

;; Go language support
(load! "config/lang-config/lang-go-config.el")

;; GraphQL language support
(load! "config/lang-config/lang-graphql-config.el")

;; JavaScript support (Node, React, etc.)
(load! "config/lang-config/lang-javascript-config.el")

;; Java language support
(load! "config/lang-config/lang-java-config.el")

;; JSON (with LSP validation)
(load! "config/lang-config/lang-json-config.el")

;; LaTeX + LSP + latexmk
(load! "config/lang-config/lang-latex-config.el")

;; Lua scripting
(load! "config/lang-config/lang-lua-config.el")

;; Markdown (Pandoc export support)
(load! "config/lang-config/lang-markdown-config.el")

;; Org-mode enhancements (roam, noter, hugo, etc.)
(load! "config/lang-config/lang-org-config.el")

;; PHP (with LSP support)
(load! "config/lang-config/lang-php-config.el")

;; PlantUML diagrams
(load! "config/lang-config/lang-plantuml-config.el")

;; REST client inside Emacs
(load! "config/lang-config/lang-rest-config.el")

;; Ruby (with Rails support)
(load! "config/lang-config/lang-ruby-config.el")

;; Scala (with LSP support)
(load! "config/lang-config/lang-scala-config.el")

;; POSIX Shell scripting (sh-mode)
(load! "config/lang-config/lang-shell-config.el")

;; Bash, Zsh, Fish support
(load! "config/lang-config/lang-sh-config.el")

;; SQL databases (Postgres, MySQL, SQLite, etc.)
(load! "config/lang-config/lang-sql-config.el")

(load! "config/lang-config/lang-ejc-sql-config.el")

;; TypeScript (with LSP support)
(load! "config/lang-config/lang-typescript-config.el")

;; Web stack (HTML, CSS, frameworks like React, Angular, Jekyll, etc.)
(load! "config/lang-config/lang-web-config.el")

;; YAML (LSP validation)
(load! "config/lang-config/lang-yaml-config.el")

(provide 'lang-module)

;;; lang-module.el ends here
