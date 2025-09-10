;;; module/config/checkers-config/checkers-grammar-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Grammar checking setup using LanguageTool.
;; Provides keybindings to check or correct the current buffer.

;;; Code:

(after! langtool
  ;; Set path to LanguageTool JAR or server
  ;; On Arch Linux (pacman install): /usr/share/languagetool/languagetool-commandline.jar
  (setq langtool-language-tool-jar "/usr/share/languagetool/languagetool-commandline.jar"
        langtool-default-language "en-US"))

;; Keybindings for grammar checking
(map! :leader
      (:prefix-map ("e" . "editor")
       (:prefix-map ("c" . "checkers")
        (:prefix-map ("g" . "grammar")
         :desc "Grammar check buffer" "g" #'langtool-check
         :desc "Grammar clear buffer" "G" #'langtool-correct-buffer))))

(provide 'checkers-grammar-config)

;;; checkers-grammar-config.el ends here
