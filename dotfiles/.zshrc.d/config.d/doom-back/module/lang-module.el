;;; module/lang-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module centralizes configuration for all programming, scripting, and
;; markup languages in Emacs. Each language or category has its own dedicated
;; config file under `config/lang-config/` to keep the setup modular and maintainable.
;;
;; Load order rationale for safety:
;; 1. Core foundational languages (C/C++, Emacs Lisp)
;; 2. Data formats (CSV, JSON, YAML, XML)
;; 3. Scripting languages (Shell, Bash, Zsh, Fish, Lua, Go)
;; 4. LSP-enabled languages (Java, TypeScript, JavaScript, PHP, Ruby, Scala)
;; 5. Markup, documents, and frameworks (Markdown, LaTeX, Org, Web)
;; 6. Specialized tools (PlantUML, REST client)
;;
;; Optional languages or modes can be safely commented/uncommented without
;; affecting other modules.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Core / foundational languages
;; ---------------------------------------------------------------------------
(load! "config/lang-config/lang-cc-config.el")       ;; C / C++
;;(load! "config/lang-config/lang-common-lisp-config.el") ;; Common Lisp
;;(load! "config/lang-config/lang-emacs-lisp-config.el")  ;; Emacs Lisp

;; ---------------------------------------------------------------------------
;; 2. Data formats
;; ---------------------------------------------------------------------------
(load! "config/lang-config/lang-data-config.el")     ;; CSV, XML, etc.
(load! "config/lang-config/lang-json-config.el")     ;; JSON validation
(load! "config/lang-config/lang-yaml-config.el")     ;; YAML validation

;; ---------------------------------------------------------------------------
;; 3. Scripting languages
;; ---------------------------------------------------------------------------
(load! "config/lang-config/lang-shell-config.el")    ;; POSIX Shell scripting
(load! "config/lang-config/lang-sh-config.el")       ;; Bash, Zsh, Fish
(load! "config/lang-config/lang-lua-config.el")      ;; Lua scripting
(load! "config/lang-config/lang-go-config.el")       ;; Go language support

;; ---------------------------------------------------------------------------
;; 4. LSP-enabled programming languages
;; ---------------------------------------------------------------------------
(load! "config/lang-config/lang-java-config.el")        ;; Java
(load! "config/lang-config/lang-javascript-config.el")  ;; JavaScript, Node, React
(load! "config/lang-config/lang-typescript-config.el")  ;; TypeScript
(load! "config/lang-config/lang-php-config.el")         ;; PHP
(load! "config/lang-config/lang-ruby-config.el")        ;; Ruby, Rails
(load! "config/lang-config/lang-scala-config.el")       ;; Scala

;; ---------------------------------------------------------------------------
;; 5. Markup, documents, and organizational tools
;; ---------------------------------------------------------------------------
(load! "config/lang-config/lang-markdown-config.el")    ;; Markdown
(load! "config/lang-config/lang-latex-config.el")       ;; LaTeX + LSP + latexmk
(load! "config/lang-config/lang-org-config.el")         ;; Org-mode enhancements

;; ---------------------------------------------------------------------------
;; 6. Specialized tools / utilities
;; ---------------------------------------------------------------------------
(load! "config/lang-config/lang-plantuml-config.el")    ;; PlantUML diagrams
(load! "config/lang-config/lang-rest-config.el")       ;; REST client
(load! "config/lang-config/lang-sql-config.el")        ;; SQL databases
(load! "config/lang-config/lang-ejc-sql-config.el")    ;; Extended SQL helpers
(load! "config/lang-config/lang-web-config.el")        ;; Web stack: HTML/CSS/JS frameworks

(provide 'lang-module)

;;; lang-module.el ends here
