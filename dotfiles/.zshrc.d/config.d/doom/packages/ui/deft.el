;; Deft  is a major mode for creating, browsing, and filtering notes written in plain text formats, such as org-mode, markdown, and LaTeX.
;; It enables you to quickly jot down thoughts and easily retrieve them later.

;; Ensure Deft is installed and ready to use
(use-package! deft
  :commands deft
  :config
  (setq deft-directory "~/Documents/doom/org/notes"
        deft-extensions "md"
        deft-recursive t
        deft-new-file-format "md"
        deft-use-filename-as-title t))

;; Keybinding to open Deft
(map! :leader
      :desc "Open Deft"
      "t D" #'deft)
