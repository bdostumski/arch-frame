;;; module/config/editor-config/editor-format-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Combined configuration for Tree-sitter and smart auto-formatting in Doom Emacs.
;; Provides:
;;  - Fast syntax highlighting and navigation via Tree-sitter
;;  - Conditional format-on-save for JS/TS, Python, Go, SQL, and Java
;;  - Leader keybindings for Tree-sitter navigation and format toggling

;;; Code:

;; ----------------------------
;; Tree-sitter: core setup
;; ----------------------------
(use-package! tree-sitter
  :hook (prog-mode . tree-sitter-mode)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ----------------------------
;; Tree-sitter languages
;; ----------------------------
(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  (setq tree-sitter-langs-grammar-dir
        (expand-file-name "tree-sitter/grammars/" doom-data-dir)
        tree-sitter-langs-async-install t))

;; ----------------------------
;; Tree-sitter keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("t" . "tree-sitter")
;;       :desc "Go to function start" "f" #'tree-sitter-goto-function-start
;;       :desc "Go to function end" "e" #'tree-sitter-goto-function-end))

;; ----------------------------
;; Smart auto-formatting
;; ----------------------------
(after! format
  ;; Prefer LSP formatting when available
  (setq +format-with-lsp t)
  ;; Disable auto-format for certain modes
  (setq +format-on-save-enabled-modes
        '(not emacs-lisp-mode makefile-mode))
  ;; Leader toggle
  ;;(map! :leader
  ;;      :desc "Toggle format on save" "t f" #'+format/toggle)
  )

;; ----------------------------
;; Helper functions for conditional formatters
;; ----------------------------
(defun my/project-root ()
  "Return current project root or nil."
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(defun my/project-has-files-p (&rest files)
  "Return non-nil if project root contains one of FILES."
  (when-let ((root (my/project-root)))
    (seq-some (lambda (f) (file-exists-p (expand-file-name f root))) files)))

;; ----------------------------
;; Language-specific conditional formatters
;; ----------------------------
;; JS/TS → Prettier
(after! (js2-mode typescript-mode)
  (defun my/js-enable-prettier ()
    (when (my/project-has-files-p ".prettierrc" "prettier.config.js" "package.json")
      (setq-local +format-with-lsp nil)
      (set-formatter! 'prettier '("prettier" "--stdin-filepath" "%file"))
      (+format-enable-on-save-h)))
  (add-hook 'js2-mode-hook #'my/js-enable-prettier)
  (add-hook 'typescript-mode-hook #'my/js-enable-prettier))

;; Python → Black
(after! python
  (defun my/python-enable-black ()
    (when (my/project-has-files-p "pyproject.toml" "setup.cfg" "black.toml")
      (set-formatter! 'black "black -q -")
      (+format-enable-on-save-h)))
  (add-hook 'python-mode-hook #'my/python-enable-black))

;; Go → gofmt
(after! go-mode
  (defun my/go-enable-gofmt ()
    (when (my/project-has-files-p "go.mod" "go.sum")
      (set-formatter! 'gofmt "gofmt")
      (+format-enable-on-save-h)))
  (add-hook 'go-mode-hook #'my/go-enable-gofmt))

;; SQL → pgformatter
(after! sql-mode
  (defun my/sql-enable-pgformatter ()
    (when (my/project-has-files-p ".sqlformat" "pg_format.conf")
      (set-formatter! 'pgformatter "pg_format -")
      (+format-enable-on-save-h)))
  (add-hook 'sql-mode-hook #'my/sql-enable-pgformatter))

;; Java → Google style / Spotless
(after! java-mode
  (defun my/java-enable-formatter ()
    (when (my/project-has-files-p "pom.xml" "build.gradle")
      (set-formatter! 'google-java "google-java-format -")
      (+format-enable-on-save-h)))
  (add-hook 'java-mode-hook #'my/java-enable-formatter))

(provide 'editor-format-config)

;;; editor-format-config.el ends here
