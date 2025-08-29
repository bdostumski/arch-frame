;;; module/config/tools-config/tools-llm-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; LLM and AI integration for Doom Emacs using Copilot.
;; Provides auto-completion, AI queries, and toggle commands.

;;; Code:

(after! llm
  ;; Enable Copilot as the backend
  (setq llm-backend '+copilot)

  ;; Optional: enable auto-complete suggestions
  (setq llm-auto-complete t))

;; Leader keybindings for LLM/AI actions
;;(map! :leader
;;      (:prefix-map ("a" . "AI")
;;       :desc "Accept suggestion" "a" #'llm-accept-suggestion
;;       :desc "Toggle Copilot" "t" #'llm-toggle
;;       :desc "Ask AI" "q" #'llm-query))

(provide 'tools-llm-config)

;;; tools-llm-config.el ends here
