;;; module/config/emacs-config/yasnippet-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Yasnippet setup with multiple snippet directories, Ivy integration,
;; and project-specific snippets.

;;; Code:

(use-package! yasnippet
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :commands yas-minor-mode
  :custom
  (yas-snippet-dirs
   '("~/.config/emacs/snippets"           ;; personal snippets
     "~/.local/share/yasnippet-snippets"  ;; community snippets
     "~/.doom.d/project-specific-snippets")) ;; project-specific snippets
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :config
  (yas-reload-all))

(use-package! yasnippet-snippets
  :after yasnippet
  :defer t)

(use-package! ivy-yasnippet
  :after (ivy yasnippet)
  :commands ivy-yasnippet
  :config
  ;; Optional keybinding for Ivy snippet selection
  (define-key yas-minor-mode-map (kbd "C-c y") #'ivy-yasnippet))

(provide 'yasnippet-config)

;;; yasnippet-config.el ends here
