;;; module/config/editor-config/editor-fold-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Universal code folding for Doom Emacs.
;; Provides Vim-style keybindings, visual indicators, and fold persistence across sessions.

;;; Code:

;; ----------------------------
;; hideshow (built-in folding)
;; ----------------------------
(use-package! hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; Prettify the folding indicator
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize " Prettify the folding indicator ..." 'face '(:inherit font-lock-comment-face)))))))

;; ----------------------------
;; Persistent folds using vimish-fold
;; ----------------------------
(use-package! vimish-fold
  :after evil
  :config
  (vimish-fold-global-mode 1))

(use-package! evil-vimish-fold
  :after (evil vimish-fold)
  :config
  (evil-vimish-fold-mode 1))

(provide 'editor-fold-config)

;;; editor-fold-config.el ends here
