;;; checkers-grammar-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Grammar checking setup using LanguageTool.
;; Provides keybindings to check or correct the current buffer.
;; Enhanced with better path detection and server support.

;;; Code:

;; Function to detect LanguageTool installation
(defun checkers-grammar--detect-languagetool ()
  "Detect LanguageTool installation and configure accordingly."
  (let ((possible-paths
         '("/usr/share/java/languagetool/languagetool-commandline.jar"  ; Arch Linux specific path
           "/usr/share/languagetool/languagetool-commandline.jar"       ; Other Linux
           "/usr/local/share/languagetool/languagetool-commandline.jar" ; Homebrew
           "/opt/languagetool/languagetool-commandline.jar"             ; Manual install
           "~/languagetool/languagetool-commandline.jar")))             ; User install
    (catch 'found
      (dolist (path possible-paths)
        (when (file-exists-p (expand-file-name path))
          (setq langtool-language-tool-jar (expand-file-name path))
          (message "LanguageTool JAR found at: %s" path)
          (throw 'found t)))
      ;; If no JAR found, check for server
      (when (executable-find "languagetool")
        (setq langtool-language-tool-server-jar nil)
        (message "LanguageTool server executable found"))
      (message "Warning: LanguageTool not found. Please install it for grammar checking."))))

(use-package langtool
  :defer t
  :init
  ;; Detect and configure LanguageTool
  (checkers-grammar--detect-languagetool)
  :config
  ;; Configuration
  (setq langtool-default-language "en-US"
        langtool-mother-tongue "en"
        ;; Disable some rules that might be too strict
        langtool-disabled-rules '("WHITESPACE_RULE"
                                  "EN_UNPAIRED_BRACKETS"
                                  "COMMA_PARENTHESIS_WHITESPACE"
                                  "EN_QUOTES"))

  ;; Custom function to check region or buffer
  (defun checkers-grammar-check-dwim ()
    "Check grammar in region if active, otherwise check buffer."
    (interactive)
    (if (use-region-p)
        (langtool-check (region-beginning) (region-end))
      (langtool-check-buffer)))

  ;; Auto-save before grammar check
  (advice-add 'langtool-check :before
              (lambda (&rest _)
                (when (and (buffer-modified-p) (buffer-file-name))
                  (save-buffer)))))

(provide 'checkers-grammar-config)
;;; checkers-grammar-config.el ends here
