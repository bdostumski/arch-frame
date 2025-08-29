;;; module/config/tools-config/tools-lookup-config.el -*- lexical-binding: t -*-
;;; Commentary:
;; Lookup and documentation integration for Doom Emacs.
;; Supports symbol definitions, references, documentation, and dictionary lookups.

;;; Code:

(after! lookup
  ;; Use dictionary for selected text or word under cursor
  (setq lookup-dictionary-dictionary "en_US"))

;; Leader keybindings for lookup actions
;;(map! :leader
;;      (:prefix-map ("l" . "lookup")
;;       :desc "Lookup symbol" "d" #'lookup/definition
;;       :desc "Lookup references" "r" #'lookup/references
;;       :desc "Lookup documentation" "h" #'lookup/documentation
;;       :desc "Lookup dictionary" "D" #'lookup/word))

(provide 'tools-lookup-config)

;;; tools-lookup-config.el ends here
