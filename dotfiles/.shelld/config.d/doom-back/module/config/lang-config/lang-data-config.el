;;; lang-data-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for data-related modes: CSV, XML, JSON, YAML and other structured data files.

;;; Code:

;; ----------------------------
;; CSV configuration
;; ----------------------------
(after! csv-mode
  (setq csv-separators '("," ";" "\t" "|"))
  (setq csv-align-style 'auto)
  
  ;; Align fields on file open and after editing
 (add-hook 'csv-mode-hook #'csv-align-fields))

;; ----------------------------
;; XML / NXML configuration
;; ----------------------------
(after! nxml-mode
  (setq nxml-child-indent 2
        nxml-attribute-indent 2
        nxml-slash-auto-complete-flag t)  ;; Auto-close XML tags
  
  ;; Better navigation and formatting
  (add-hook 'nxml-mode-hook #'turn-on-auto-fill)
  (add-hook 'nxml-mode-hook #'hs-minor-mode)  ;; Enable code folding
  )
  
;; ----------------------------
;; JSON configuration
;; ----------------------------
(after! json-mode
  (setq js-indent-level 2)
  
  (add-hook 'json-mode-hook (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2)))
  
  ;; Format JSON with jq if available
  (when (executable-find "jq")
    (defun jq-format-json ()
      "Format JSON using jq."
      (interactive)
      (let ((cmd "jq . "))
        (shell-command-on-region (point-min) (point-max) cmd nil t)))))

;; ----------------------------
;; YAML configuration
;; ----------------------------
(after! yaml-mode
  (setq yaml-indent-offset 2)
  
  ;; Enable auto-pairing of quotes and brackets
  (add-hook 'yaml-mode-hook #'electric-pair-mode))

(provide 'lang-data-config)

;;; lang-data-config.el ends here
