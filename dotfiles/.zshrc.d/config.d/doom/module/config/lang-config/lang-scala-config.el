;;; module/config/lang-config/lang-scala-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Scala configuration for Doom Emacs.
;; Enables LSP, sets indentation, and provides leader keybindings for REPL, tests,
;; navigation, and formatting.

;;; Code:

(after! scala-mode
  ;; Enable LSP in Scala buffers
  (add-hook 'scala-mode-hook #'lsp)

  ;; Optional: set indentation
  (setq scala-indent:step 2))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("s" . "scala")
;;       :desc "Run REPL" "r" #'scala-run-repl
;;       :desc "Run tests" "t" #'sbt-test
;;       :desc "Go to definition" "d" #'lsp-find-definition
;;       :desc "Format buffer" "f" #'lsp-format-buffer))

(provide 'lang-scala-config)

;;; lang-scala-config.el ends here
