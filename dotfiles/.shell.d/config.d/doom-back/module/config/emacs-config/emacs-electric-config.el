;;; module/config/emacs-config/emacs-electric-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive electric indentation and auto-formatting configuration for Doom Emacs.
;; This module provides intelligent automatic indentation, electric pairs, and formatting.

;;; Code:

;;; Basic Electric Indentation
;; Enable electric-indent-mode globally (affects all buffers)
(electric-indent-mode +1)

;; Enable electric-pair-mode for automatic bracket/quote pairing
(electric-pair-mode +1)

;; Enable electric-layout-mode for automatic newline insertion
(electric-layout-mode +1)

;;; Advanced Electric Indentation Settings
;; Customize electric indent behavior
(setq electric-indent-inhibit-functions
      '(electric-indent-inhibit-for-comments
        electric-indent-inhibit-for-strings))

;; Characters that trigger electric indentation
(setq electric-indent-chars
      (append electric-indent-chars
              '(?\; ?\, ?\) ?\] ?\} ?| ?& ?: ?= ?< ?> ?+ ?- ?* ?/ ?% ?!)))

;;; Electric Pair Configuration
;; Customize electric pair behavior
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\' . ?\')
        (?\` . ?\`)
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})))

;; Skip whitespace when closing pairs
(setq electric-pair-skip-whitespace t)

;; Skip self when inserting pairs
(setq electric-pair-skip-self t)

;; Preserve balance when deleting
(setq electric-pair-delete-adjacent-pairs t)

;;; Electric Layout Configuration
;; Define electric layout rules for automatic newline insertion
(setq electric-layout-rules
      '((?\; . after)    ; Semicolon adds newline after
        (?\{ . after-stay) ; Opening brace adds newline but keeps cursor
        (?\} . before)))   ; Closing brace adds newline before

;;; Mode-Specific Electric Configuration
;; Programming modes
(dolist (hook '(prog-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                scheme-mode-hook
                c-mode-hook
                c++-mode-hook
                java-mode-hook
                python-mode-hook
                ruby-mode-hook
                perl-mode-hook
                php-mode-hook
                js-mode-hook
                typescript-mode-hook
                web-mode-hook
                css-mode-hook
                scss-mode-hook
                rust-mode-hook
                go-mode-hook
                haskell-mode-hook
                clojure-mode-hook
                erlang-mode-hook
                elixir-mode-hook))
  (add-hook hook #'electric-indent-local-mode)
  (add-hook hook #'electric-pair-local-mode)
  (add-hook hook #'electric-layout-local-mode))

;; Text and markup modes (selective electric features)
(dolist (hook '(text-mode-hook
                markdown-mode-hook
                org-mode-hook
                latex-mode-hook
                html-mode-hook
                xml-mode-hook
                yaml-mode-hook
                json-mode-hook))
  (add-hook hook #'electric-pair-local-mode))

;;; Language-Specific Electric Pairs
;; Add language-specific electric pairs
(defun +electric/setup-language-pairs ()
  "Setup language-specific electric pairs."
  (cond
   ;; LaTeX mode
   ((derived-mode-p 'latex-mode 'LaTeX-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs
                        '((?$ . ?$)))))

   ;; Org mode
   ((derived-mode-p 'org-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs
                        '((?= . ?=)
                          (?~ . ?~)
                          (?+ . ?+)))))

   ;; Markdown mode
   ((derived-mode-p 'markdown-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs
                        '((?* . ?*)
                          (?_ . ?_)
                          (?` . ?`)))))

   ;; C/C++ modes
   ((derived-mode-p 'c-mode 'c++-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs
                        '((?< . ?>)))))

   ;; Web modes (HTML, JSX, etc.)
   ((derived-mode-p 'web-mode 'html-mode 'jsx-mode 'tsx-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs
                        '((?< . ?>)))))))

;; Apply language-specific pairs
(add-hook 'after-change-major-mode-hook #'+electric/setup-language-pairs)

;;; Smart Electric Indentation Functions
(defun +electric/smart-newline-and-indent ()
  "Insert newline and indent smartly based on context."
  (interactive)
  (let ((point-before (point)))
    (newline-and-indent)
    ;; If we're between matching pairs, add extra indentation
    (when (and (> point-before (point-min))
               (< point-before (point-max))
               (eq (char-before point-before) ?\{)
               (eq (char-after point-before) ?\}))
      (save-excursion
        (forward-line -1)
        (end-of-line)
        (newline-and-indent)))))

(defun +electric/smart-semicolon ()
  "Insert semicolon and handle indentation smartly."
  (interactive)
  (if (derived-mode-p 'c-mode 'c++-mode 'java-mode 'js-mode 'typescript-mode)
      (progn
        (insert ";")
        (when (looking-at "\\s-*$")
          (newline-and-indent)))
    (insert ";")))

(defun +electric/smart-closing-brace ()
  "Insert closing brace with smart indentation."
  (interactive)
  (insert "}")
  (indent-according-to-mode)
  (when (and (looking-at "\\s-*$")
             (save-excursion
               (forward-line 1)
               (not (eobp))))
    (newline-and-indent)))

;;; Electric Quote Configuration
;; Enable smart quotes in text modes
(when (fboundp 'electric-quote-mode)
  (add-hook 'text-mode-hook #'electric-quote-local-mode)
  (add-hook 'org-mode-hook #'electric-quote-local-mode)
  (add-hook 'markdown-mode-hook #'electric-quote-local-mode)

  ;; Customize quote characters
  (setq electric-quote-chars '(?' ?' ?\" ?\"))
  (setq electric-quote-paragraph t)
  (setq electric-quote-comment t)
  (setq electric-quote-string nil))

;;; Electric Indentation Helpers
(defun +electric/toggle-electric-indent ()
  "Toggle electric indent mode on/off."
  (interactive)
  (if electric-indent-mode
      (progn
        (electric-indent-mode -1)
        (message "Electric indent disabled"))
    (progn
      (electric-indent-mode +1)
      (message "Electric indent enabled"))))

(defun +electric/toggle-electric-pair ()
  "Toggle electric pair mode on/off."
  (interactive)
  (if electric-pair-mode
      (progn
        (electric-pair-mode -1)
        (message "Electric pair disabled"))
    (progn
      (electric-pair-mode +1)
      (message "Electric pair enabled"))))

(defun +electric/disable-in-minibuffer ()
  "Disable electric modes in minibuffer."
  (setq-local electric-indent-mode nil)
  (setq-local electric-pair-mode nil))

;; Disable electric modes in minibuffer
(add-hook 'minibuffer-setup-hook #'+electric/disable-in-minibuffer)

;;; Auto-format on Save (Optional)
;; Uncomment the following section if you want automatic formatting on save

;; (defun +electric/format-buffer-on-save ()
;;   "Format buffer automatically on save."
;;   (when (and (derived-mode-p 'prog-mode)
;;              (not (derived-mode-p 'fundamental-mode)))
;;     (ignore-errors
;;       (indent-region (point-min) (point-max)))))

;; (add-hook 'before-save-hook #'+electric/format-buffer-on-save)

;;; Whitespace and Cleanup
;; Clean up whitespace with electric indentation
(defun +electric/cleanup-whitespace ()
  "Clean up trailing whitespace and tabs."
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)
    (when (and (not indent-tabs-mode)
               (save-excursion
                 (goto-char (point-min))
                 (search-forward "\t" nil t)))
      (untabify (point-min) (point-max)))))

;; Optional: Enable whitespace cleanup on save
;; (add-hook 'before-save-hook #'+electric/cleanup-whitespace)

;;; Integration with Doom Emacs
;; If using with Doom Emacs modules
(after! smartparens
  ;; Disable smartparens in favor of electric-pair-mode in some cases
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp))))

;; Integration with aggressive-indent-mode
(after! aggressive-indent
  ;; Exclude certain modes from aggressive indenting
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode))

;;; Performance Optimization
;; Optimize electric indentation for large files
(defvar +electric-large-file-threshold 100000
  "Threshold for disabling electric features in large files.")

(defun +electric/check-file-size ()
  "Disable electric features for large files."
  (when (and buffer-file-name
             (> (buffer-size) +electric-large-file-threshold))
    (electric-indent-local-mode -1)
    (electric-pair-local-mode -1)
    (electric-layout-local-mode -1)
    (message "Electric features disabled for large file")))

(add-hook 'find-file-hook #'+electric/check-file-size)

;;; Custom Commands
(defun +electric/electric-newline-between-pairs ()
  "Insert newline between matching pairs and indent properly."
  (interactive)
  (when (and (eq (char-before) ?\{)
             (eq (char-after) ?\}))
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (indent-according-to-mode))))

;;; Provide the module
(provide 'emacs-electric-config)

;;; emacs-electric-config.el ends here
