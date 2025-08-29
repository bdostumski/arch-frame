;;; module/config/lang-config/lang-plantuml-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; PlantUML configuration for Doom Emacs.
;; Sets the PlantUML jar path, execution mode, and leader keybindings
;; for previewing and exporting UML diagrams.

;;; Code:

(after! plantuml-mode
  ;; Set PlantUML jar location
  (setq plantuml-jar-path "~/.config/emacs/.local/etc/plantuml.jar"
        plantuml-default-exec-mode 'jar))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("u" . "uml")
;;       :desc "Preview diagram" "p" #'plantuml-preview
;;       :desc "Export diagram" "e" #'plantuml-export-buffer))

(provide 'lang-plantuml-config)

;;; lang-plantuml-config.el ends here
