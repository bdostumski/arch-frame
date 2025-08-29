;;; module/config/lang-config/lang-data-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for data-related modes: CSV, XML, and other structured data files.

;;; Code:

;; ----------------------------
;; CSV configuration
;; ----------------------------
(after! csv-mode
  (setq csv-separators '("," ";" "\t"))
  (add-hook 'csv-mode-hook #'csv-align-fields))

;; ----------------------------
;; XML / NXML configuration
;; ----------------------------
(after! nxml-mode
  (setq nxml-child-indent 2
        nxml-attribute-indent 2)
  (add-hook 'nxml-mode-hook #'turn-on-auto-fill))

(provide 'lang-data-config)

;;; lang-data-config.el ends here
