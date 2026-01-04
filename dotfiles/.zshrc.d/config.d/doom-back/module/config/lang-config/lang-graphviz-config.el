;;; lang-graphviz-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Graphviz DOT language support with preview and export keybindings.

;;; Code:

(after! graphviz-dot-mode
  ;; Set default DOT command
  (setq graphviz-dot-command "dot"))

;; Leader keybindings for Graphviz
(map! :leader
      (:prefix-map ("g" . "graphviz")
       :desc "Preview graph" "p" #'graphviz-dot-preview
       :desc "Export graph"  "e" #'graphviz-dot-export-buffer))

(provide 'lang-graphviz-config)

;;; lang-graphviz-config.el ends here
