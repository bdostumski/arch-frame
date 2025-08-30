;;; module/config/lang-config/lang-common-lisp-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Common Lisp configuration with LSP, REPL integration, and useful keybindings.

;;; Code:

(use-package! lisp-mode
  ;; Enable LSP in Lisp buffers
  (add-hook 'lisp-mode-hook #'lsp)

  ;; Configure REPL integration
  (setq lisp-program "sbcl"           ;; choose your preferred Lisp implementation
        lisp-repl-buffer-name "*lisp-repl*"))

;; Leader keybindings for Lisp
;;(map! :leader
;;      (:prefix-map ("l" . "Lisp")
;;       :desc "Start REPL"       "r" #'run-lisp
;;       :desc "Eval last sexp"   "e" #'eval-last-sexp
;;       :desc "Eval buffer"      "b" #'eval-buffer))

(provide 'lang-common-lisp-config)

;;; lang-common-lisp-config.el ends here
