;;; module/config/emacs-config/emacs-ibuffer-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Comprehensive Ibuffer configuration for Doom Emacs.
;; Features advanced grouping, sorting, formatting, and productivity enhancements.

;;; Code:

;; Core Ibuffer Settings
(use-package! ibuffer
  :config
  ;; Don't show empty groups
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Add VC status column
  (define-ibuffer-column vc-status-mini
    (:name "VC")
    (if (and buffer-file-name (vc-backend buffer-file-name))
        (let ((state (vc-state buffer-file-name)))
          (cond
           ((eq state 'edited) "✎")
           ((eq state 'added) "✚")
           ((eq state 'removed) "✖")
           ((eq state 'missing) "✗")
           ((eq state 'ignored) "◌")
           ((eq state 'unregistered) "?")
           (t "✓")))
      " "))

  ;; Custom format for buffer list
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
           (name 25 25 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 12 12 :left)
           " "
           filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))

  ;; Comprehensive filter groups
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("System Buffers" (or (name . "^\\*scratch\\*$")
                                 (name . "^\\*Messages\\*$")
                                 (name . "^\\*Backtrace\\*$")
                                 (name . "^\\*Compile-Log\\*$")
                                 (name . "^\\*Completions\\*$")
                                 (name . "^\\*Help\\*$")
                                 (name . "^\\*info\\*$")
                                 (name . "^\\*Apropos\\*$")
                                 (name . "^\\*Buffer List\\*$")
                                 (name . "^\\*Kill Ring\\*$")
                                 (name . "^\\*Occur\\*$")
                                 (name . "^\\*grep\\*$")
                                 (name . "^\\*Compile\\*$")))

           ("Doom Buffers" (or (name . "^\\*doom")
                               (name . "^\\*straight")
                               (name . "^\\*Package")
                               (name . "^\\*quelpa")
                               (name . "^\\*use-package")))

           ("Org Mode" (or (mode . org-mode)
                           (mode . org-agenda-mode)
                           (name . "^\\*Org")
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")))

           ("Magit" (or (mode . magit-status-mode)
                        (mode . magit-log-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-revision-mode)
                        (mode . magit-blame-mode)
                        (name . "^\\*magit")))

           ("Version Control" (or (mode . diff-mode)
                                  (mode . vc-dir-mode)
                                  (name . "^\\*vc")))

           ("Dired" (mode . dired-mode))

           ("Web Development" (or (mode . web-mode)
                                  (mode . html-mode)
                                  (mode . css-mode)
                                  (mode . scss-mode)
                                  (mode . sass-mode)
                                  (mode . js-mode)
                                  (mode . js2-mode)
                                  (mode . typescript-mode)
                                  (mode . json-mode)
                                  (mode . yaml-mode)))

           ("JavaScript/TypeScript" (or (mode . js-mode)
                                        (mode . js2-mode)
                                        (mode . javascript-mode)
                                        (mode . typescript-mode)
                                        (mode . tsx-mode)
                                        (mode . json-mode)))

           ("Python" (or (mode . python-mode)
                         (mode . inferior-python-mode)
                         (name . "^\\*Python\\*$")))

           ("Lisp" (or (mode . emacs-lisp-mode)
                       (mode . lisp-mode)
                       (mode . scheme-mode)
                       (mode . clojure-mode)
                       (mode . common-lisp-mode)
                       (name . "^\\*slime")))

           ("Shell/Terminal" (or (mode . shell-mode)
                                 (mode . eshell-mode)
                                 (mode . term-mode)
                                 (mode . vterm-mode)
                                 (name . "^\\*shell\\*$")
                                 (name . "^\\*eshell\\*$")
                                 (name . "^\\*terminal\\*$")
                                 (name . "^\\*vterm")))

           ("Documentation" (or (mode . Info-mode)
                                (mode . help-mode)
                                (mode . helpful-mode)
                                (mode . Man-mode)
                                (mode . woman-mode)
                                (name . "^\\*Help\\*$")
                                (name . "^\\*Man ")
                                (name . "^\\*WoMan ")))

           ("LSP" (or (name . "^\\*lsp")
                      (name . "^\\*eglot")
                      (mode . lsp-mode)
                      (mode . eglot-mode)))

           ("Treemacs" (or (mode . treemacs-mode)
                           (name . "^\\*Treemacs")))

           ("Compilation" (or (mode . compilation-mode)
                              (mode . grep-mode)
                              (name . "^\\*compilation\\*$")
                              (name . "^\\*grep\\*$")
                              (name . "^\\*Async Shell Command\\*$")))

           ("Programming" (and (derived-mode . prog-mode)
                               (not (starred-name))))

           ("Text Files" (and (derived-mode . text-mode)
                              (not (starred-name))))

           ("Special" (starred-name)))))

  ;; Sorting preferences
  (setq ibuffer-default-sorting-mode 'major-mode)

  ;; Auto-update settings
  (setq ibuffer-auto-update-interval 2)

  ;; Hooks for automatic setup
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-auto-mode 1)
              ;; Enable hl-line-mode for better visibility
              (hl-line-mode 1)
              ;; Hide empty groups
              (setq ibuffer-show-empty-filter-groups nil))))

;; Enhanced Ibuffer functions
(defun +ibuffer/ibuffer-kill-all-buffers-in-group ()
  "Kill all buffers in the current filter group."
  (interactive)
  (let ((group-name (get-text-property (point) 'ibuffer-filter-group-name)))
    (when group-name
      (when (yes-or-no-p (format "Really kill all buffers in group '%s'? " group-name))
        (let ((buffers (ibuffer-current-filter-buffers)))
          (dolist (buffer buffers)
            (kill-buffer buffer))
          (message "Killed %d buffers in group '%s'" (length buffers) group-name)
          (ibuffer-update nil t))))))

(defun +ibuffer/ibuffer-toggle-grouping ()
  "Toggle between grouped and ungrouped buffer list."
  (interactive)
  (if ibuffer-filter-groups
      (progn
        (setq ibuffer-filter-groups nil)
        (message "Ibuffer grouping disabled"))
    (ibuffer-switch-to-saved-filter-groups "default")
    (message "Ibuffer grouping enabled")))

(defun +ibuffer/ibuffer-visit-buffer-other-window-and-close ()
  "Visit buffer in other window and close ibuffer."
  (interactive)
  (ibuffer-visit-buffer-other-window)
  (kill-buffer "*Ibuffer*"))

(defun +ibuffer/ibuffer-create-new-group ()
  "Create a new filter group interactively."
  (interactive)
  (let* ((group-name (read-string "Group name: "))
         (filter-type (completing-read "Filter type: "
                                       '("mode" "name" "filename" "size-gt" "size-lt")))
         (filter-value (read-string (format "%s filter value: " filter-type))))
    (add-to-list 'ibuffer-saved-filter-groups
                 `(,group-name
                   (,(intern filter-type) . ,filter-value)) t)
    (ibuffer-switch-to-saved-filter-groups "default")
    (message "Created group '%s'" group-name)))

;; Additional useful functions
(defun +ibuffer/ibuffer-refresh-and-update ()
  "Refresh ibuffer and update filter groups."
  (interactive)
  (when (eq major-mode 'ibuffer-mode)
    (ibuffer-update nil t)
    (ibuffer-switch-to-saved-filter-groups "default")))

;; Auto-refresh ibuffer when switching between buffers
(defvar +ibuffer/last-buffer nil)
(defun +ibuffer/ibuffer-auto-refresh ()
  "Auto-refresh ibuffer when buffer changes."
  (unless (eq (current-buffer) +ibuffer/last-buffer)
    (setq +ibuffer/last-buffer (current-buffer))
    (when (get-buffer "*Ibuffer*")
      (with-current-buffer "*Ibuffer*"
        (ibuffer-update nil t)))))

;; Hook to enable auto-refresh (optional - may cause performance issues)
;; (add-hook 'buffer-list-update-hook #'+ibuffer/ibuffer-auto-refresh)

;; Integration with projectile (if available)
(when (featurep 'projectile)
  (defun +ibuffer/ibuffer-projectile ()
    "Open ibuffer filtered by current projectile project."
    (interactive)
    (if (projectile-project-p)
        (let ((project-root (projectile-project-root)))
          (ibuffer)
          (ibuffer-filter-by-filename (concat "^" (regexp-quote project-root))))
      (ibuffer))))

;; Custom face for modified buffers
(custom-set-faces
 '(ibuffer-modified-mark ((t (:foreground "#ff6c6b" :weight bold)))))

(provide 'emacs-ibuffer-config)

;;; emacs-ibuffer-config.el ends here
