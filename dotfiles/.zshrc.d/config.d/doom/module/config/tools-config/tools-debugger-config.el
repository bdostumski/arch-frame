;;; module/config/tools-config/tools-debugger-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Debugging configuration for Doom Emacs using DAP (Debug Adapter Protocol).
;; Integrates with LSP-enabled languages and provides UI enhancements,
;; keybindings, and example adapters for Java and Node.js.

;;; Code:

;; ----------------------------
;; DAP-mode setup
;; ----------------------------
(after! dap-mode
  ;; Enable UI and tooltip enhancements
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  ;; Load language-specific adapters
  (require 'dap-java)  ;; Java debugging (requires lsp-java)
  (require 'dap-node)) ;; Node.js debugging (requires vscode-js-debug)

;; ----------------------------
;; Leader keybindings for debugging
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("d" . "debug")
;;       :desc "Debug last session" "l" #'dap-debug-last
;;       :desc "Restart debug session" "r" #'dap-debug-restart
;;       :desc "Step over" "o" #'dap-next
;;       :desc "Step into" "i" #'dap-step-in
;;       :desc "Step out" "u" #'dap-step-out
;;       :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle))

(provide 'tools-debugger-config)

;;; tools-debugger-config.el ends here
