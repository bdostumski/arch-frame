;;; module/config/lang-config/lang-json-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; JSON setup with LSP, formatting, and convenient keybindings.

;;; Code:

(after! json-mode
  ;; Enable LSP in JSON buffers
  (add-hook 'json-mode-hook #'lsp)

  ;; Formatting options
  (setq js-indent-level 2
        json-reformat:indent-width 2))

;; Leader keybindings for JSON
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "lang")
                                (:prefix-map ("j" . "json")
                                 :desc "Format buffer"   "f" #'json-reformat-region
                                 :desc "Validate buffer" "v" #'lsp-format-buffer))))

(provide 'lang-json-config)

;;; lang-json-config.el ends here
