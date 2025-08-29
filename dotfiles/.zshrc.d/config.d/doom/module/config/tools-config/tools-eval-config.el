;;; module/config/tools-config/tools-eval-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for inline evaluation of Emacs Lisp expressions.
;; Supports overlays for inline results and leader keybindings for quick evaluation.

;;; Code:

(after! eval
  ;; Show evaluation results inline
  (setq +eval-display-style 'overlay
        +eval-overlay-duration 3))  ;; duration in seconds

;; Leader keybindings for evaluation commands
;;(map! :leader
;;      (:prefix-map ("e" . "eval")
;;       :desc "Eval expression" "e" #'eval-last-sexp
;;       :desc "Eval buffer" "b" #'eval-buffer
;;       :desc "Eval region" "r" #'eval-region
;;       :desc "Eval defun" "d" #'eval-defun))

(provide 'tools-eval-config)

;;; tools-eval-config.el ends here
