;;; lang-scala-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Scala configuration for Doom Emacs.
;; Enables LSP, sets indentation, and provides leader keybindings for REPL, tests,
;; navigation, and formatting.

;;; Code:

(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

(after! scala-mode
  ;; Enable LSP in Scala buffers
  (add-hook 'scala-mode-hook #'lsp)

  ;; Set indentation
  (setq scala-indent:step 2)
  
  ;; Additional scala settings
  (setq scala-indent:align-forms t)
  (setq scala-indent:align-parameters t)
  (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy))

;; Set up sbt-mode for running SBT
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Define sbt-run-repl if not available
(defun sbt-run-repl ()
  "Start the Scala REPL in sbt."
  (interactive)
  (sbt:command "console"))

(provide 'lang-scala-config)

;;; lang-scala-config.el ends here
