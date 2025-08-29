;;; module/config/emacs-config/emacs-ibuffer-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Ibuffer configuration for Doom Emacs.
;; Automatically groups buffers by type and updates dynamically.

;;; Code:

;; Define saved filter groups
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired"        (mode . dired-mode))
         ("Org"          (mode . org-mode))
         ("Emacs"        (or (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")))
         ("Programming"  (mode . prog-mode)))))

;; Automatically apply filter groups and enable auto-update
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-auto-mode 1)))

;; Optional: Keybinding for quick access (SPC b i in Doom)
(map! :leader
      :desc "Open Ibuffer" "b i" #'ibuffer)

(provide 'emacs-ibuffer-config)

;;; emacs-ibuffer-config.el ends here
