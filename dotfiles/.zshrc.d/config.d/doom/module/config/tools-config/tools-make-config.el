;;; module/config/tools-config/tools-make-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Make integration for Doom Emacs.
;; Provides default make command, buffer display, and leader keybindings
;; for running and managing Make tasks.

;;; Code:

(after! make
  ;; Default make command
  (setq +make-command "make -k")  ;; continue on errors

  ;; Display compilation buffer automatically
  (setq +make-display-buffer t))

;; ----------------------------
;; Leader keybindings for Make
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("m" . "make")
;;       :desc "Run make" "r" #'+make/run
;;       :desc "Run last make command" "l" #'+make/run-last
;;       :desc "View compilation buffer" "b" #'+make/view-buffer))

(provide 'tools-make-config)

;;; tools-make-config.el ends here
