;;; module/config/os-config/os-tty-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; TTY (terminal) specific enhancements for Doom Emacs.
;; Enables improvements when running Emacs in a non-graphical environment.

;;; Code:

(when (not (display-graphic-p))
  ;; Example: improve terminal scrolling
  (setq scroll-margin 0
        scroll-conservatively 10000
        scroll-step 1
        visible-cursor nil)  ;; optional: hide cursor in TTY if desired

  ;; Term-mode tweaks
  (with-eval-after-load 'term
    (setq term-scroll-show-maximum-output t
          term-bind-key-alist nil)))

(provide 'os-tty-config)

;;; os-tty-config.el ends here
