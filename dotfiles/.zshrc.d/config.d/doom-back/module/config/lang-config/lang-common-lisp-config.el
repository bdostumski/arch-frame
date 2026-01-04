;;; lang-common-lisp-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Common Lisp configuration with LSP, SLIME integration, and useful keybindings.

;;; Code:

;; Main Common Lisp configuration
(use-package! lisp-mode
  :hook (lisp-mode . lsp)
  :config
  ;; Configure preferred Lisp implementation
  (setq inferior-lisp-program "sbcl"))

;; SLIME - Superior Lisp Interaction Mode for Emacs
(use-package! slime
  :after lisp-mode
  :config
  (setq slime-contribs '(slime-fancy slime-asdf slime-quicklisp slime-company)
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-net-coding-system 'utf-8-unix)
  
  ;; Auto-start SLIME when opening a Lisp file (optional)
  ;; (add-hook 'lisp-mode-hook (lambda () (unless (slime-connected-p) (slime))))
  )

;; LSP configuration for Common Lisp
(use-package! lsp-mode
  :when (modulep! :tools lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("cl-lsp"))
                    :activation-fn (lsp-activate-on "lisp")
                    :server-id 'cl-lsp))
  (add-to-list 'lsp-language-id-configuration '(lisp-mode . "lisp")))

;; Company configuration for Lisp
(use-package! company
  :when (modulep! :completion company)
  :config
  (add-hook 'lisp-mode-hook (lambda ()
                              (setq-local company-idle-delay 0.1
                                          company-minimum-prefix-length 2))))

;; ParEdit for structural editing of Lisp
(use-package! paredit
  :hook (lisp-mode . paredit-mode)
  :config
  (show-paren-mode 1))

;; Rainbow delimiters for better visibility of nested parentheses
(use-package! rainbow-delimiters
  :hook (lisp-mode . rainbow-delimiters-mode))

(provide 'lang-common-lisp-config)
;;; lang-common-lisp-config.el ends here
