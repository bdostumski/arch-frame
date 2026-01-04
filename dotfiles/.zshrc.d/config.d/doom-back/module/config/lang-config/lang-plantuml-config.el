;;; lang-plantuml-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; PlantUML configuration for Doom Emacs.
;; Sets the PlantUML jar path, execution mode, and leader keybindings
;; for previewing and exporting UML diagrams.

;;; Code:

;; Define variables first so they exist before plantuml-mode is loaded
(setq-default plantuml-jar-path "~/.config/emacs/.local/etc/plantuml.jar"
              plantuml-default-exec-mode 'jar)

;; Additional configuration after plantuml-mode is loaded
(after! plantuml-mode
  ;; Any additional plantuml-mode specific configurations can go here
  )

(provide 'lang-plantuml-config)

;;; lang-plantuml-config.el ends here
