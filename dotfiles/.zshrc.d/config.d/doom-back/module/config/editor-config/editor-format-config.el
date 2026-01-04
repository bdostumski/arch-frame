;;; module/config/editor-config/editor-format-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive configuration for Tree-sitter and smart auto-formatting in Doom Emacs.
;; Provides:
;;  - Fast syntax highlighting and navigation via Tree-sitter
;;  - Conditional format-on-save for 20+ programming languages
;;  - Leader keybindings for Tree-sitter navigation and format toggling
;;  - Robust error handling and performance optimizations
;;  - Project-specific configuration detection

;;; Code:

;; ----------------------------
;; Core variables and utilities
;; ----------------------------
(defvar +editor-format/format-cache (make-hash-table :test 'equal)
  "Cache for formatter availability checks.")

(defvar +editor-format/format-log-buffer "*Format Log*"
  "Buffer name for formatting operation logs.")

(defvar +editor-format/format-timeout 10
  "Timeout in seconds for formatting operations.")

(defun +editor-format/log-format-action (action &optional details)
  "Log formatting ACTION with optional DETAILS."
  (when (get-buffer-create +editor-format/format-log-buffer)
    (with-current-buffer +editor-format/format-log-buffer
      (goto-char (point-max))
      (insert (format "[%s] %s%s\n"
                      (format-time-string "%H:%M:%S")
                      action
                      (if details (format ": %s" details) ""))))))

(defun +editor-format/executable-find-cached (program)
  "Find PROGRAM in PATH with caching."
  (or (gethash program +editor-format/format-cache)
      (puthash program (executable-find program) +editor-format/format-cache)))

(defun +editor-format/project-root ()
  "Return current project root with fallback mechanisms."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      (and (fboundp 'project-root) (when-let ((proj (project-current))) (project-root proj)))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun +editor-format/project-has-files-p (&rest files)
  "Return non-nil if project root contains one of FILES."
  (when-let ((root (+editor-format/project-root)))
    (seq-some (lambda (f)
                (let ((path (expand-file-name f root)))
                  (or (file-exists-p path)
                      (file-exists-p (concat path ".json"))
                      (file-exists-p (concat path ".js"))
                      (file-exists-p (concat path ".yaml"))
                      (file-exists-p (concat path ".yml")))))
              files)))

(defun +editor-format/buffer-has-content-p (patterns)
  "Check if current buffer contains any of PATTERNS."
  (save-excursion
    (goto-char (point-min))
    (seq-some (lambda (pattern)
                (re-search-forward pattern nil t))
              patterns)))

(defun +editor-format/safe-format-buffer (formatter)
  "Safely format buffer with FORMATTER, handling errors gracefully."
  (condition-case err
      (progn
        (+editor-format/log-format-action "Formatting started" (symbol-name formatter))
        (funcall formatter)
        (+editor-format/log-format-action "Formatting completed" (symbol-name formatter))
        t)
    (error
     (+editor-format/log-format-action "Formatting failed"
                                       (format "%s: %s" (symbol-name formatter) (error-message-string err)))
     nil)))

;; ----------------------------
;; Tree-sitter: core setup
;; ----------------------------
(use-package! tree-sitter
  :hook (prog-mode . tree-sitter-mode)
  :init
  (setq tree-sitter-debug-mode nil
        tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (+editor-format/log-format-action "Tree-sitter initialized"))

;; ----------------------------
;; Tree-sitter languages
;; ----------------------------
(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  (setq tree-sitter-langs-grammar-dir
        (expand-file-name "tree-sitter/grammars/" doom-data-dir)
        tree-sitter-langs-async-install t)

  ;; Ensure grammars directory exists
  (unless (file-directory-p tree-sitter-langs-grammar-dir)
    (make-directory tree-sitter-langs-grammar-dir t))

  (+editor-format/log-format-action "Tree-sitter languages configured"))

;; ----------------------------
;; Smart auto-formatting
;; ----------------------------
(after! format
  ;; Prefer LSP formatting when available
  (setq +format-with-lsp t
        +format-preserve-indentation t
        +format-region-p t)

  ;; Disable auto-format for certain modes
  (setq +format-on-save-enabled-modes
        '(not emacs-lisp-mode
          makefile-mode
          yaml-mode
          markdown-mode
          org-mode
          text-mode)))

;; ----------------------------
;; JavaScript/TypeScript → Multiple formatters
;; ----------------------------
(after! (js2-mode typescript-mode rjsx-mode web-mode)
  (defun +editor-format/js-enable-formatter ()
    "Enable appropriate formatter for JS/TS files."
    (cond
     ;; Prettier (preferred)
     ((and (+editor-format/executable-find-cached "prettier")
           (+editor-format/project-has-files-p ".prettierrc" "prettier.config" "package.json"))
      (setq-local +format-with-lsp nil)
      (set-formatter! 'prettier '("prettier" "--stdin-filepath" "%file"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Prettier enabled for JS/TS"))

     ;; ESLint with --fix
     ((and (+editor-format/executable-find-cached "eslint")
           (+editor-format/project-has-files-p ".eslintrc" "eslint.config"))
      (setq-local +format-with-lsp nil)
      (set-formatter! 'eslint '("eslint" "--fix" "--stdin" "--stdin-filename" "%file"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "ESLint enabled for JS/TS"))

     ;; Standard JS
     ((and (+editor-format/executable-find-cached "standard")
           (+editor-format/project-has-files-p "package.json"))
      (setq-local +format-with-lsp nil)
      (set-formatter! 'standard '("standard" "--fix" "--stdin"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Standard JS enabled"))

     ;; Fall back to LSP
     (t (+editor-format/log-format-action "Using LSP formatter for JS/TS"))))

  (add-hook 'js2-mode-hook #'+editor-format/js-enable-formatter)
  (add-hook 'typescript-mode-hook #'+editor-format/js-enable-formatter)
  (add-hook 'rjsx-mode-hook #'+editor-format/js-enable-formatter)
  (add-hook 'web-mode-hook #'+editor-format/js-enable-formatter))

;; ----------------------------
;; Python → Multiple formatters
;; ----------------------------
(after! python
  (defun +editor-format/python-enable-formatter ()
    "Enable appropriate formatter for Python files."
    (cond
     ;; Black (preferred for formatting)
     ((and (+editor-format/executable-find-cached "black")
           (+editor-format/project-has-files-p "pyproject.toml" "setup.cfg" ".black"))
      (set-formatter! 'black '("black" "--quiet" "--stdin-filename" "%file" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Black enabled for Python"))

     ;; autopep8
     ((and (+editor-format/executable-find-cached "autopep8")
           (+editor-format/project-has-files-p "setup.cfg" "pyproject.toml"))
      (set-formatter! 'autopep8 '("autopep8" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "autopep8 enabled for Python"))

     ;; yapf
     ((and (+editor-format/executable-find-cached "yapf")
           (+editor-format/project-has-files-p ".style.yapf" "setup.cfg"))
      (set-formatter! 'yapf '("yapf"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "yapf enabled for Python"))

     ;; isort for imports
     ((and (+editor-format/executable-find-cached "isort")
           (+editor-format/project-has-files-p "pyproject.toml" ".isort.cfg"))
      (set-formatter! 'isort '("isort" "--stdout" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "isort enabled for Python"))

     (t (+editor-format/log-format-action "Using LSP formatter for Python"))))

  (add-hook 'python-mode-hook #'+editor-format/python-enable-formatter))

;; ----------------------------
;; Rust → rustfmt
;; ----------------------------
(after! (rustic rust-mode)
  (defun +editor-format/rust-enable-formatter ()
    "Enable rustfmt for Rust files."
    (when (and (+editor-format/executable-find-cached "rustfmt")
               (+editor-format/project-has-files-p "Cargo.toml" "rustfmt.toml"))
      (setq-local +format-with-lsp nil)
      (set-formatter! 'rustfmt '("rustfmt" "--edition" "2021"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "rustfmt enabled for Rust")))

  (add-hook 'rustic-mode-hook #'+editor-format/rust-enable-formatter)
  (add-hook 'rust-mode-hook #'+editor-format/rust-enable-formatter))

;; ----------------------------
;; Go → Multiple formatters
;; ----------------------------
(after! go-mode
  (defun +editor-format/go-enable-formatter ()
    "Enable appropriate formatter for Go files."
    (when (+editor-format/project-has-files-p "go.mod" "go.sum")
      (cond
       ;; goimports (preferred - handles imports + formatting)
       ((+editor-format/executable-find-cached "goimports")
        (set-formatter! 'goimports '("goimports"))
        (+format-enable-on-save-h)
        (+editor-format/log-format-action "goimports enabled for Go"))

       ;; gofmt (fallback)
       ((+editor-format/executable-find-cached "gofmt")
        (set-formatter! 'gofmt '("gofmt"))
        (+format-enable-on-save-h)
        (+editor-format/log-format-action "gofmt enabled for Go"))

       ;; gofumpt (stricter gofmt)
       ((+editor-format/executable-find-cached "gofumpt")
        (set-formatter! 'gofumpt '("gofumpt"))
        (+format-enable-on-save-h)
        (+editor-format/log-format-action "gofumpt enabled for Go")))))

  (add-hook 'go-mode-hook #'+editor-format/go-enable-formatter))

;; ----------------------------
;; C/C++ → clang-format
;; ----------------------------
(after! (c-mode c++-mode objc-mode)
  (defun +editor-format/c-enable-formatter ()
    "Enable clang-format for C/C++ files."
    (when (and (+editor-format/executable-find-cached "clang-format")
               (or (+editor-format/project-has-files-p ".clang-format" "_clang-format")
                   (+editor-format/buffer-has-content-p '("ClangFormat" "clang-format"))))
      (set-formatter! 'clang-format '("clang-format" "-assume-filename" "%file"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "clang-format enabled for C/C++")))

  (add-hook 'c-mode-hook #'+editor-format/c-enable-formatter)
  (add-hook 'c++-mode-hook #'+editor-format/c-enable-formatter)
  (add-hook 'objc-mode-hook #'+editor-format/c-enable-formatter))

;; ----------------------------
;; PHP → Multiple formatters
;; ----------------------------
(after! php-mode
  (defun +editor-format/php-enable-formatter ()
    "Enable appropriate formatter for PHP files."
    (cond
     ;; PHP-CS-Fixer
     ((and (+editor-format/executable-find-cached "php-cs-fixer")
           (+editor-format/project-has-files-p ".php-cs-fixer.php" ".php_cs"))
      (set-formatter! 'php-cs-fixer '("php-cs-fixer" "fix" "--using-cache=no" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "PHP-CS-Fixer enabled"))

     ;; phpcbf (PHP Code Beautifier and Fixer)
     ((and (+editor-format/executable-find-cached "phpcbf")
           (+editor-format/project-has-files-p "phpcs.xml" ".phpcs.xml"))
      (set-formatter! 'phpcbf '("phpcbf" "--stdin-path" "%file" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "phpcbf enabled"))

     ;; PHP Intelephense formatter
     (t (+editor-format/log-format-action "Using LSP formatter for PHP"))))

  (add-hook 'php-mode-hook #'+editor-format/php-enable-formatter))

;; ----------------------------
;; Ruby → Multiple formatters
;; ----------------------------
(after! ruby-mode
  (defun +editor-format/ruby-enable-formatter ()
    "Enable appropriate formatter for Ruby files."
    (cond
     ;; RuboCop
     ((and (+editor-format/executable-find-cached "rubocop")
           (+editor-format/project-has-files-p ".rubocop.yml" "Gemfile"))
      (set-formatter! 'rubocop '("rubocop" "--auto-correct" "--stdin" "%file" "--format" "quiet"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "RuboCop enabled"))

     ;; Standard Ruby
     ((and (+editor-format/executable-find-cached "standardrb")
           (+editor-format/project-has-files-p "Gemfile"))
      (set-formatter! 'standardrb '("standardrb" "--fix" "--stdin" "%file"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "StandardRB enabled"))

     ;; Prettier for Ruby
     ((and (+editor-format/executable-find-cached "rbprettier")
           (+editor-format/project-has-files-p "Gemfile"))
      (set-formatter! 'rbprettier '("rbprettier" "--stdin-filename" "%file"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "rbprettier enabled"))))

  (add-hook 'ruby-mode-hook #'+editor-format/ruby-enable-formatter))

;; ----------------------------
;; Java → Multiple formatters
;; ----------------------------
(after! java-mode
  (defun +editor-format/java-enable-formatter ()
    "Enable appropriate formatter for Java files."
    (cond
     ;; Google Java Format
     ((and (+editor-format/executable-find-cached "google-java-format")
           (+editor-format/project-has-files-p "pom.xml" "build.gradle" ".java-version"))
      (set-formatter! 'google-java-format '("google-java-format" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Google Java Format enabled"))

     ;; Eclipse formatter
     ((and (+editor-format/executable-find-cached "eclipse")
           (+editor-format/project-has-files-p "eclipse-format.xml"))
      (set-formatter! 'eclipse-java
        '("eclipse" "-application" "org.eclipse.jdt.core.JavaCodeFormatter"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Eclipse formatter enabled"))

     ;; Spotless (Gradle/Maven plugin)
     (t (+editor-format/log-format-action "Using LSP formatter for Java"))))

  (add-hook 'java-mode-hook #'+editor-format/java-enable-formatter))

;; ----------------------------
;; Kotlin → ktlint
;; ----------------------------
(after! kotlin-mode
  (defun +editor-format/kotlin-enable-formatter ()
    "Enable ktlint for Kotlin files."
    (when (and (+editor-format/executable-find-cached "ktlint")
               (+editor-format/project-has-files-p "build.gradle" "build.gradle.kts"))
      (set-formatter! 'ktlint '("ktlint" "--format" "--stdin"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "ktlint enabled for Kotlin")))

  (add-hook 'kotlin-mode-hook #'+editor-format/kotlin-enable-formatter))

;; ----------------------------
;; Swift → swift-format
;; ----------------------------
(after! swift-mode
  (defun +editor-format/swift-enable-formatter ()
    "Enable swift-format for Swift files."
    (when (and (+editor-format/executable-find-cached "swift-format")
               (+editor-format/project-has-files-p "Package.swift" ".swift-format"))
      (set-formatter! 'swift-format '("swift-format"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "swift-format enabled for Swift")))

  (add-hook 'swift-mode-hook #'+editor-format/swift-enable-formatter))

;; ----------------------------
;; Scala → scalafmt
;; ----------------------------
(after! scala-mode
  (defun +editor-format/scala-enable-formatter ()
    "Enable scalafmt for Scala files."
    (when (and (+editor-format/executable-find-cached "scalafmt")
               (+editor-format/project-has-files-p ".scalafmt.conf" "build.sbt"))
      (set-formatter! 'scalafmt '("scalafmt" "--stdin"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "scalafmt enabled for Scala")))

  (add-hook 'scala-mode-hook #'+editor-format/scala-enable-formatter))

;; ----------------------------
;; Dart → dartfmt
;; ----------------------------
(after! dart-mode
  (defun +editor-format/dart-enable-formatter ()
    "Enable dartfmt for Dart files."
    (when (and (+editor-format/executable-find-cached "dartfmt")
               (+editor-format/project-has-files-p "pubspec.yaml" "pubspec.yml"))
      (set-formatter! 'dartfmt '("dartfmt"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "dartfmt enabled for Dart")))

  (add-hook 'dart-mode-hook #'+editor-format/dart-enable-formatter))

;; ----------------------------
;; Elixir → mix format
;; ----------------------------
(after! elixir-mode
  (defun +editor-format/elixir-enable-formatter ()
    "Enable mix format for Elixir files."
    (when (and (+editor-format/executable-find-cached "mix")
               (+editor-format/project-has-files-p "mix.exs" ".formatter.exs"))
      (set-formatter! 'mix-format '("mix" "format" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "mix format enabled for Elixir")))

  (add-hook 'elixir-mode-hook #'+editor-format/elixir-enable-formatter))

;; ----------------------------
;; Erlang → erlfmt
;; ----------------------------
(after! erlang
  (defun +editor-format/erlang-enable-formatter ()
    "Enable erlfmt for Erlang files."
    (when (and (+editor-format/executable-find-cached "erlfmt")
               (+editor-format/project-has-files-p "rebar.config" "erlang.mk"))
      (set-formatter! 'erlfmt '("erlfmt" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "erlfmt enabled for Erlang")))

  (add-hook 'erlang-mode-hook #'+editor-format/erlang-enable-formatter))

;; ----------------------------
;; Haskell → ormolu/brittany
;; ----------------------------
(after! haskell-mode
  (defun +editor-format/haskell-enable-formatter ()
    "Enable appropriate formatter for Haskell files."
    (cond
     ;; Ormolu (preferred)
     ((and (+editor-format/executable-find-cached "ormolu")
           (+editor-format/project-has-files-p "package.yaml" "*.cabal"))
      (set-formatter! 'ormolu '("ormolu"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "ormolu enabled for Haskell"))

     ;; Brittany
     ((and (+editor-format/executable-find-cached "brittany")
           (+editor-format/project-has-files-p "package.yaml" "*.cabal"))
      (set-formatter! 'brittany '("brittany"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "brittany enabled for Haskell"))

     ;; stylish-haskell
     ((+editor-format/executable-find-cached "stylish-haskell")
      (set-formatter! 'stylish-haskell '("stylish-haskell"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "stylish-haskell enabled for Haskell"))))

  (add-hook 'haskell-mode-hook #'+editor-format/haskell-enable-formatter))

;; ----------------------------
;; OCaml → ocamlformat
;; ----------------------------
(after! (tuareg caml-mode)
  (defun +editor-format/ocaml-enable-formatter ()
    "Enable ocamlformat for OCaml files."
    (when (and (+editor-format/executable-find-cached "ocamlformat")
               (+editor-format/project-has-files-p "dune-project" ".ocamlformat"))
      (set-formatter! 'ocamlformat '("ocamlformat" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "ocamlformat enabled for OCaml")))

  (add-hook 'tuareg-mode-hook #'+editor-format/ocaml-enable-formatter)
  (add-hook 'caml-mode-hook #'+editor-format/ocaml-enable-formatter))

;; ----------------------------
;; F# → fantomas
;; ----------------------------
(after! fsharp-mode
  (defun +editor-format/fsharp-enable-formatter ()
    "Enable fantomas for F# files."
    (when (and (+editor-format/executable-find-cached "fantomas")
               (+editor-format/project-has-files-p "*.fsproj" "paket.dependencies"))
      (set-formatter! 'fantomas '("fantomas" "--stdin"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "fantomas enabled for F#")))

  (add-hook 'fsharp-mode-hook #'+editor-format/fsharp-enable-formatter))

;; ----------------------------
;; SQL → Multiple formatters
;; ----------------------------
(after! sql
  (defun +editor-format/sql-enable-formatter ()
    "Enable appropriate formatter for SQL files."
    (cond
     ;; pg_format (PostgreSQL)
     ((and (+editor-format/executable-find-cached "pg_format")
           (+editor-format/project-has-files-p ".sqlformat" "pg_format.conf"))
      (set-formatter! 'pgformatter '("pg_format" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "pg_format enabled"))

     ;; sqlformat (from sqlparse)
     ((+editor-format/executable-find-cached "sqlformat")
      (set-formatter! 'sqlformat '("sqlformat" "--reindent" "--keywords" "upper" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "sqlformat enabled"))

     ;; sql-formatter
     ((+editor-format/executable-find-cached "sql-formatter")
      (set-formatter! 'sql-formatter '("sql-formatter"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "sql-formatter enabled"))))

  (add-hook 'sql-mode-hook #'+editor-format/sql-enable-formatter))

;; ----------------------------
;; CSS/SCSS/LESS → Prettier/stylelint
;; ----------------------------
(after! (css-mode scss-mode less-css-mode)
  (defun +editor-format/css-enable-formatter ()
    "Enable appropriate formatter for CSS/SCSS/LESS files."
    (cond
     ;; Prettier
     ((and (+editor-format/executable-find-cached "prettier")
           (+editor-format/project-has-files-p ".prettierrc" "package.json"))
      (set-formatter! 'prettier-css '("prettier" "--parser" "css"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Prettier enabled for CSS"))

     ;; stylelint
     ((and (+editor-format/executable-find-cached "stylelint")
           (+editor-format/project-has-files-p ".stylelintrc" "stylelint.config"))
      (set-formatter! 'stylelint '("stylelint" "--fix" "--stdin-filename" "%file"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "stylelint enabled for CSS"))))

  (add-hook 'css-mode-hook #'+editor-format/css-enable-formatter)
  (add-hook 'scss-mode-hook #'+editor-format/css-enable-formatter)
  (add-hook 'less-css-mode-hook #'+editor-format/css-enable-formatter))

;; ----------------------------
;; HTML → Prettier/tidy
;; ----------------------------
(after! (html-mode mhtml-mode)
  (defun +editor-format/html-enable-formatter ()
    "Enable appropriate formatter for HTML files."
    (cond
     ;; Prettier
     ((and (+editor-format/executable-find-cached "prettier")
           (+editor-format/project-has-files-p ".prettierrc" "package.json"))
      (set-formatter! 'prettier-html '("prettier" "--parser" "html"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Prettier enabled for HTML"))

     ;; tidy
     ((+editor-format/executable-find-cached "tidy")
      (set-formatter! 'tidy '("tidy" "-q" "--show-warnings" "no"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "tidy enabled for HTML"))))

  (add-hook 'html-mode-hook #'+editor-format/html-enable-formatter)
  (add-hook 'mhtml-mode-hook #'+editor-format/html-enable-formatter))

;; ----------------------------
;; XML → xmllint/tidy
;; ----------------------------
(after! nxml-mode
  (defun +editor-format/xml-enable-formatter ()
    "Enable appropriate formatter for XML files."
    (cond
     ;; xmllint
     ((+editor-format/executable-find-cached "xmllint")
      (set-formatter! 'xmllint '("xmllint" "--format" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "xmllint enabled for XML"))

     ;; tidy for XML
     ((+editor-format/executable-find-cached "tidy")
      (set-formatter! 'tidy-xml '("tidy" "-xml" "-i" "-q"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "tidy enabled for XML"))))

  (add-hook 'nxml-mode-hook #'+editor-format/xml-enable-formatter))

;; ----------------------------
;; YAML → prettier/yamlfmt
;; ----------------------------
(after! yaml-mode
  (defun +editor-format/yaml-enable-formatter ()
    "Enable formatter for YAML files."
    (cond
     ;; Prettier
     ((and (+editor-format/executable-find-cached "prettier")
           (+editor-format/project-has-files-p ".prettierrc" "package.json"))
      (set-formatter! 'prettier-yaml '("prettier" "--parser" "yaml"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Prettier enabled for YAML"))

     ;; yamlfmt
     ((+editor-format/executable-find-cached "yamlfmt")
      (set-formatter! 'yamlfmt '("yamlfmt" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "yamlfmt enabled for YAML"))))

  (add-hook 'yaml-mode-hook #'+editor-format/yaml-enable-formatter))

;; ----------------------------
;; JSON → prettier/jq
;; ----------------------------
(after! json-mode
  (defun +editor-format/json-enable-formatter ()
    "Enable formatter for JSON files."
    (cond
     ;; Prettier
     ((and (+editor-format/executable-find-cached "prettier")
           (+editor-format/project-has-files-p ".prettierrc" "package.json"))
      (set-formatter! 'prettier-json '("prettier" "--parser" "json"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "Prettier enabled for JSON"))

     ;; jq
     ((+editor-format/executable-find-cached "jq")
      (set-formatter! 'jq '("jq" "."))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "jq enabled for JSON"))))

  (add-hook 'json-mode-hook #'+editor-format/json-enable-formatter))

;; ----------------------------
;; TOML → taplo
;; ----------------------------
(after! toml-mode
  (defun +editor-format/toml-enable-formatter ()
    "Enable taplo for TOML files."
    (when (+editor-format/executable-find-cached "taplo")
      (set-formatter! 'taplo '("taplo" "format" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "taplo enabled for TOML")))

  (add-hook 'toml-mode-hook #'+editor-format/toml-enable-formatter))

;; ----------------------------
;; Protocol Buffers → clang-format
;; ----------------------------
(after! protobuf-mode
  (defun +editor-format/protobuf-enable-formatter ()
    "Enable clang-format for Protocol Buffer files."
    (when (+editor-format/executable-find-cached "clang-format")
      (set-formatter! 'clang-format-proto '("clang-format" "-assume-filename=file.proto"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "clang-format enabled for Protocol Buffers")))

  (add-hook 'protobuf-mode-hook #'+editor-format/protobuf-enable-formatter))

;; ----------------------------
;; Docker → hadolint (linting)
;; ----------------------------
(after! dockerfile-mode
  (defun +editor-format/docker-enable-formatter ()
    "Enable hadolint for Dockerfile files."
    (when (+editor-format/executable-find-cached "hadolint")
      (+editor-format/log-format-action "hadolint available for Dockerfile linting")))

  (add-hook 'dockerfile-mode-hook #'+editor-format/docker-enable-formatter))

;; ----------------------------
;; Terraform → terraform fmt
;; ----------------------------
(after! terraform-mode
  (defun +editor-format/terraform-enable-formatter ()
    "Enable terraform fmt for Terraform files."
    (when (and (+editor-format/executable-find-cached "terraform")
               (+editor-format/project-has-files-p "*.tf" "terraform.tfvars"))
      (set-formatter! 'terraform-fmt '("terraform" "fmt" "-"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "terraform fmt enabled")))

  (add-hook 'terraform-mode-hook #'+editor-format/terraform-enable-formatter))

;; ----------------------------
;; Nix → nixfmt
;; ----------------------------
(after! nix-mode
  (defun +editor-format/nix-enable-formatter ()
    "Enable nixfmt for Nix files."
    (when (+editor-format/executable-find-cached "nixfmt")
      (set-formatter! 'nixfmt '("nixfmt"))
      (+format-enable-on-save-h)
      (+editor-format/log-format-action "nixfmt enabled for Nix")))

  (add-hook 'nix-mode-hook #'+editor-format/nix-enable-formatter))

;; ----------------------------
;; Auto-cleanup and optimization
;; ----------------------------
(defun +editor-format/cleanup-format-cache ()
  "Clean up stale entries in format cache."
  (maphash (lambda (key value)
             (unless (and value (file-executable-p value))
               (remhash key +editor-format/format-cache)))
           +editor-format/format-cache))

;; Run cleanup periodically (every 5 minutes)
(run-with-timer 300 300 #'+editor-format/cleanup-format-cache)

;; ----------------------------
;; Project-specific overrides
;; ----------------------------
(defun +editor-format/load-project-format-config ()
  "Load project-specific format configuration if available."
  (when-let* ((root (+editor-format/project-root))
              (config-file (expand-file-name ".emacs-format.el" root)))
    (when (file-exists-p config-file)
      (condition-case err
          (progn
            (load-file config-file)
            (+editor-format/log-format-action "Loaded project format config" config-file))
        (error (+editor-format/log-format-action "Failed to load project config"
                                                 (error-message-string err)))))))

;; Load project config when opening files
(add-hook 'find-file-hook #'+editor-format/load-project-format-config)

;; ----------------------------
;; Initialization
;; ----------------------------
(+editor-format/log-format-action "Comprehensive editor format configuration loaded"
                                  (format "Emacs %s, Doom %s" emacs-version
                                          (if (boundp 'doom-version) doom-version "unknown")))

(provide 'editor-format-config)

;;; editor-format-config.el ends here
