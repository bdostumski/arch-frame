;;; module/config/ui-config/ui-popup-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure Doom's popup system to use childframes and sane defaults for temporary windows.

;;; Code:

(after! popup
  ;; Default popup settings
  (setq +popup-defaults '((side . bottom)       ;; default position
                          (size . 0.3)          ;; relative size
                          (slot . 0)            ;; stacking order
                          (vslot . 0)
                          (quit . t)            ;; ESC closes popup
                          (select . t)          ;; auto-select popup
                          (autosave . t)))      ;; save popup session

  ;; Use childframe if supported
  (setq +popup-use-childframe t))

(provide 'ui-popup-config)

;;; ui-popup-config.el ends here
