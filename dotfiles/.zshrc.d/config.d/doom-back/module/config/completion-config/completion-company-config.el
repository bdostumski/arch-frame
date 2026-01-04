;;; Commentary:
;; Comprehensive Company setup for auto-completion with enhanced UI features,
;; performance optimizations, and intelligent sorting/filtering.
;; Includes company-box icons, prescient sorting, and backend configurations.

;;; Code:

;; ----------------------------
;; Company: core completion engine
;; ----------------------------
(use-package! company
  :defer t
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("C-v" . company-next-page)
        ("M-v" . company-previous-page)
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("C-c C-y" . company-yasnippet))
  :custom
  ;; Performance optimizations
  (company-minimum-prefix-length 1)           ;; start completion after 1 char
  (company-idle-delay 0.1)                    ;; fast pop-up (0.0 for instant)
  (company-show-quick-access t)               ;; show numbers for quick selection
  (company-require-match nil)                 ;; allow free text input

  ;; UI improvements
  (company-tooltip-align-annotations t)       ;; align annotations to the right
  (company-tooltip-limit 12)                 ;; limit tooltip items
  (company-tooltip-minimum 6)                ;; minimum tooltip items
  (company-selection-wrap-around t)          ;; circular navigation
  (company-tooltip-flip-when-above t)       ;; flip tooltip when above cursor

  ;; Search and matching
  (company-search-regexp-function 'company-search-flex-regexp)
  (company-backends-in-buffer-functions '(company--categorize-backends))

  ;; Performance tweaks
  (company-async-timeout 5)                  ;; async completion timeout
  (company-echo-truncate-lines nil)          ;; don't truncate echo area

  ;; Transformers for better sorting
  (company-transformers '(company-sort-by-occurrence
                          company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix))

  :config
  ;; Add custom backends for better completion
  (setq company-backends
        '((company-capf company-yasnippet)     ;; LSP + snippets
          (company-dabbrev-code company-keywords company-files)
          company-dabbrev))

  ;; Improve company-dabbrev
  (setq company-dabbrev-downcase nil          ;; preserve case
        company-dabbrev-ignore-case nil       ;; case sensitive
        company-dabbrev-other-buffers t       ;; search other buffers
        company-dabbrev-code-other-buffers 'all)

  ;; Better file completion
  (setq company-files-exclusions '(".git/" ".DS_Store" "*.elc" "*.pyc"))

  ;; Enable company in programming modes
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'text-mode-hook #'company-mode))

;; ----------------------------
;; Company-Box: UI enhancements
;; ----------------------------
(use-package! company-box
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-max-candidates 50)
  (company-box-show-single-candidate t)
  (company-box-backends-colors nil)          ;; use theme colors
  (company-box-doc-delay 0.3)               ;; delay for documentation
  (company-box-scrollbar nil)               ;; disable scrollbar for cleaner look

  :config
  ;; Configure company-box icons for different backends
  (setq company-box-icons-unknown 'fa_question_circle)

  ;; Customize icons for different completion types
  (setq company-box-icons-elisp
        '((fa_tag :face font-lock-function-name-face) ; function
          (fa_cog :face font-lock-variable-name-face) ; variable
          (fa_cube :face font-lock-constant-face)))   ; constant

  ;; Performance: reduce company-box frame updates
  (setq company-box-frame-behavior 'point))

;; ----------------------------
;; Company-Prescient: intelligent sorting
;; ----------------------------
(use-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode)
  :custom
  (prescient-sort-length-enable nil)         ;; don't sort by length
  (prescient-aggressive-file-save t)         ;; save frequently used items
  (prescient-use-char-folding t)            ;; better fuzzy matching
  (prescient-use-case-folding 'smart)       ;; smart case matching

  :config
  ;; Save prescient data across sessions
  (prescient-persist-mode 1))

;; ----------------------------
;; Company-Quickhelp: documentation popups
;; ----------------------------
(use-package! company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.5)             ;; delay before showing help
  (company-quickhelp-max-lines 30)          ;; max lines in help popup
  (company-quickhelp-use-propertized-text t)) ;; better text rendering

;; ----------------------------
;; Company-Statistics: usage-based sorting
;; ----------------------------
(use-package! company-statistics
  :after company
  :hook (company-mode . company-statistics-mode)
  :custom
  (company-statistics-file (concat doom-cache-dir "company-statistics.el")))

;; ----------------------------
;; Language-specific enhancements
;; ----------------------------

;; Better completion for shell scripts
(use-package! company-shell
  :after company
  :config
  (add-to-list 'company-backends 'company-shell))

;; Math symbol completion
(use-package! company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode))

;; Web development completion
(use-package! company-web
  :after company
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(company-web-html company-files company-dabbrev)))))

;; ----------------------------
;; Performance optimizations
;; ----------------------------

;; Optimize company for large projects
(defun +company/optimize-for-large-projects ()
  "Optimize Company settings for large projects."
  (setq-local company-idle-delay 0.2)        ;; slightly slower in large projects
  (setq-local company-minimum-prefix-length 2) ;; require more chars
  (setq-local company-dabbrev-other-buffers nil)) ;; don't search other buffers

;; Apply optimizations for specific modes
(add-hook 'c++-mode-hook #'+company/optimize-for-large-projects)
(add-hook 'java-mode-hook #'+company/optimize-for-large-projects)

;; ----------------------------
;; Custom functions and utilities
;; ----------------------------

(defun +company/complete-common-or-cycle ()
  "Complete common prefix or cycle through candidates."
  (interactive)
  (if (and (company-tooltip-visible-p)
           (= company-candidates-length 1))
      (company-complete-selection)
    (company-complete-common-or-cycle)))

(defun +company/backend-with-yas (backends)
  "Add yasnippet to company BACKENDS."
  (if (and (listp backends) (memq 'company-yasnippet backends))
      backends
    (append (if (consp backends)
                backends
              (list backends))
            '(:with company-yasnippet))))

;; Apply yasnippet integration to all backends
(setq company-backends
      (mapcar #'+company/backend-with-yas company-backends))

;; ----------------------------
;; Mode-specific configurations
;; ----------------------------

;; Emacs Lisp specific settings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-capf company-elisp company-yasnippet)
                          company-dabbrev-code
                          company-files))))

;; Org mode completion
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local company-backends
                        '((company-capf company-yasnippet)
                          company-dabbrev
                          company-files))))

;; ----------------------------
;; Integration with other Doom modules
;; ----------------------------

;; LSP integration (if using LSP)
(after! lsp-mode
  (setq lsp-completion-provider :company))

;; Corfu fallback (if user switches to corfu later)
(defvar +company-disabled-modes '(shell-mode eshell-mode term-mode)
  "Modes where company should be disabled in favor of other completion.")

(dolist (mode +company-disabled-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (company-mode -1))))

(provide 'completion-company-config)

;;; completion-company-config.el ends here
