;;; module/config/ui-config/ui-hl-todo-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Highlight TODO/FIXME/BUG/HACK keywords in programming modes.

;;; Code:

(use-package! hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; Customize keyword colors
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF6347")
          ("FIXME"  . "#FFFF00")
          ("BUG"    . "#FF4500")
          ("HACK"   . "#8A2BE2")
          ("NOTE"   . "#87CEEB")
          ("REVIEW" . "#DAA520"))))

(provide 'ui-hl-todo-config)

;;; ui-hl-todo-config.el ends here
