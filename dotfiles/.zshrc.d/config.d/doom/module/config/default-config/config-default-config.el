;;; module/config/default-config/config-default-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Default configuration for global editor enhancements.
;; Enables smartparens globally with useful keybindings.

;;; Code:

;; ----------------------------
;; Smartparens: automatic parentheses management
;; ----------------------------
(after! smartparens
  ;; Enable smartparens globally
  (smartparens-global-mode +1)
  ;; Highlight matching pairs
  (show-smartparens-global-mode +1)
  ;; Optional: enable strict mode for programming buffers
  ;; (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  )

;; ----------------------------
;; Keybindings for smartparens
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("s" . "smartparens")
;;       :desc "Wrap region in parentheses" "w" #'sp-wrap-round
;;       :desc "Slurp forward" "f" #'sp-forward-slurp-sexp
;;       :desc "Barf forward" "b" #'sp-forward-barf-sexp
;;       :desc "Slurp backward" "F" #'sp-backward-slurp-sexp
;;       :desc "Barf backward" "B" #'sp-backward-barf-sexp))

(provide 'config-default-config)

;;; config-default-config.el ends here
