;;; module/config/tools-config/tools-pdf-config.el -*- lexical-binding: t -*-

;;; Commentary:
;; Complete PDF viewing and annotation configuration for Doom Emacs
;; Optimized for developers and researchers on Arch Linux

;;; Code:

;; ----------------------------
;; Safety Guards and Performance
;; ----------------------------
(defvar +pdf-tools-installed nil
  "Flag indicating whether pdf-tools is installed.")

;; Increase limits for large PDFs
(setq max-lisp-eval-depth 10000
      max-specpdl-size 10000)

(setq pdf-info-epdfinfo-program
      "~/.config/emacs/.local/straight/repos/pdf-tools/server/epdfinfo")

;; Make sure epdfinfo is executable
(when (file-exists-p pdf-info-epdfinfo-program)
  (set-file-modes pdf-info-epdfinfo-program #o755))

;; ----------------------------
;; Core PDF Tools Configuration
;; ----------------------------
(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-tools-handle-upgrades nil)
  :config
  ;; Core settings
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1
        pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-view-continuous nil
        pdf-cache-image-limit 32
        pdf-cache-prefetch-pages-function #'pdf-cache-prefetch-next-page
        pdf-render-text-annotations t)

  ;; Theme integration
  (setq pdf-view-midnight-colors '("#c5c8c6" . "#1d1f21"))

  ;; Disable incompatible modes
  (setq pdf-view-incompatible-modes
        '(linum-mode
          linum-relative-mode
          helm-linum-relative-mode
          nlinum-mode
          nlinum-hl-mode
          nlinum-relative-mode
          yalinum-mode)))

;; ----------------------------
;; Safe PDF-tools Installation
;; ----------------------------
(unless (featurep 'pdf-tools)
  (condition-case err
      (progn
        (message "🔄 Installing PDF-tools...")
        (pdf-tools-install :no-query)
        (message "✅ PDF-tools installed successfully"))
    (error
     (message "❌ Installation failed: %s" (error-message-string err))
     (message "💡 Ensure system dependencies are installed: sudo pacman -S poppler-glib cairo glib2"))))

;; ----------------------------
;; PDF Annotations
;; ----------------------------
(use-package! pdf-annot
  :after pdf-tools
  :config
  (setq pdf-annot-activate-created-annotations t
        pdf-annot-default-annotation-properties
        '((t (label . "bdostumski"))
          (text (icon . "Note") (color . "#ffa500"))
          (highlight (color . "#ffff00"))
          (squiggly (color . "#ff6347"))
          (strike-out (color . "#ff0000"))
          (underline (color . "#00ff00")))))

;; ----------------------------
;; PDF History
;; ----------------------------
(use-package! pdf-history
  :after pdf-tools
  :config
  (pdf-history-minor-mode 1)
  (setq pdf-history-size 100
        pdf-history-debug nil))

;; ----------------------------
;; Navigation Helpers
;; ----------------------------
(defun +pdf/safe-check ()
  "Check if current buffer is a PDF."
  (and (derived-mode-p 'pdf-view-mode) (buffer-file-name)))

(defun +pdf/fit-width () (interactive) (when (+pdf/safe-check) (pdf-view-fit-width-to-window)))
(defun +pdf/fit-height () (interactive) (when (+pdf/safe-check) (pdf-view-fit-height-to-window)))
(defun +pdf/fit-page () (interactive) (when (+pdf/safe-check) (pdf-view-fit-page-to-window)))
(defun +pdf/zoom-reset () (interactive) (when (+pdf/safe-check) (pdf-view-scale-reset)))
(defun +pdf/goto-page () (interactive) (when (+pdf/safe-check) (call-interactively #'pdf-view-goto-page)))
(defun +pdf/search-forward () (interactive) (when (+pdf/safe-check) (call-interactively #'pdf-occur)))
(defun +pdf/search-backward () (interactive) (when (+pdf/safe-check) (pdf-occur-previous-match)))

;; ----------------------------
;; Annotations Management
;; ----------------------------
(defun +pdf/add-text-annotation ()
  (interactive) (when (+pdf/safe-check)
                  (let ((note (read-string "Annotation text: ")))
                    (pdf-annot-add-text-annotation (pdf-view-current-page) nil note))))

(defun +pdf/add-highlight ()
  (interactive) (when (+pdf/safe-check)
                  (if (pdf-view-active-region-p)
                      (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region))
                    (message "No active region to highlight"))))

(defun +pdf/add-underline ()
  (interactive) (when (+pdf/safe-check)
                  (if (pdf-view-active-region-p)
                      (pdf-annot-add-underline-markup-annotation (pdf-view-active-region))
                    (message "No active region to underline"))))

(defun +pdf/add-strikeout ()
  (interactive) (when (+pdf/safe-check)
                  (if (pdf-view-active-region-p)
                      (pdf-annot-add-strikeout-markup-annotation (pdf-view-active-region))
                    (message "No active region to strike out"))))

(defun +pdf/list-annotations ()
  (interactive) (when (+pdf/safe-check) (pdf-annot-list-annotations)))

(defun +pdf/save-annotations ()
  (interactive) (when (+pdf/safe-check)
                  (pdf-annot-save-annotations)
                  (message "✅ Annotations saved")))

(defun +pdf/export-annotations ()
  (interactive)
  (when (+pdf/safe-check)
    (let* ((pdf-file (buffer-file-name))
           (txt-file (concat (file-name-sans-extension pdf-file) "-annotations.txt"))
           (annotations (pdf-annot-getannots)))
      (with-temp-file txt-file
        (insert (format "# Annotations for %s\n\n" (file-name-nondirectory pdf-file)))
        (dolist (annot annotations)
          (let ((page (pdf-annot-get annot 'page))
                (type (pdf-annot-get-type annot))
                (contents (pdf-annot-get annot 'contents)))
            (insert (format "## Page %d - %s\n" page (capitalize (symbol-name type))))
            (when contents (insert (format "**Note:** %s\n" contents)))
            (insert "---\n"))))
      (message "✅ Annotations exported: %s" txt-file)
      (when (y-or-n-p "Open exported file? ") (find-file txt-file)))))

;; ----------------------------
;; Study Tools
;; ----------------------------
(defun +pdf/create-study-notes ()
  (interactive)
  (when (+pdf/safe-check)
    (let* ((pdf-file (buffer-file-name))
           (notes-file (concat (file-name-sans-extension pdf-file) "-notes.md")))
      (with-current-buffer (find-file-noselect notes-file)
        (when (= (buffer-size) 0)
          (insert (format "# Study Notes: %s\n\n" (file-name-nondirectory pdf-file)))
          (insert "**Key Points**\n- \n\n**Quotes**\n> \n\n**Questions**\n- [ ] \n\n"))
        (display-buffer (current-buffer))))))

(defun +pdf/bookmark-current-page ()
  (interactive) (when (+pdf/safe-check)
                  (let ((page (pdf-view-current-page))
                        (note (read-string "Bookmark note: ")))
                    (pdf-annot-add-text-annotation page nil note)
                    (message "🔖 Page %d bookmarked" page))))

(defun +pdf/capture-to-org ()
  (interactive) (when (+pdf/safe-check)
                  (let ((page (pdf-view-current-page))
                        (file (buffer-file-name)))
                    (kill-new (format "[[file:%s::%d][%s - Page %d]]"
                                      file page
                                      (file-name-nondirectory file) page))
                    (message "📎 PDF link copied"))))

;; ----------------------------
;; Utilities
;; ----------------------------
(defun +pdf/extract-pages ()
  (interactive) (when (+pdf/safe-check)
                  (if (executable-find "pdftk")
                      (let* ((start (read-number "Start page: " (pdf-view-current-page)))
                             (end (read-number "End page: " start))
                             (input (buffer-file-name))
                             (output (read-file-name "Output file: "
                                                     (file-name-directory input)
                                                     nil nil
                                                     (format "%s-pages-%d-%d.pdf"
                                                             (file-name-sans-extension
                                                              (file-name-nondirectory input))
                                                             start end))))
                        (if (= 0 (shell-command (format "pdftk '%s' cat %d-%d output '%s'"
                                                        input start end output)))
                            (message "✅ Pages extracted to %s" output)
                          (message "❌ Extraction failed")))
                    (message "❌ pdftk not found. Install: sudo pacman -S pdftk"))))

;; ----------------------------
;; PDF Mode Hooks
;; ----------------------------
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (auto-revert-mode 1)
            (setq-local cursor-type nil)
            (+pdf/fit-page)
            (+pdf/sync-with-theme)
            (message "📄 PDF mode loaded")))

;; ----------------------------
;; Keybindings (Complete)
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("f" . "pdf")
                                 :desc "Go to page" "g" #'+pdf/goto-page
                                 :desc "Search forward" "/" #'+pdf/search-forward
                                 :desc "Search & highlight" "h" #'+pdf/search-forward
                                 :desc "Outline" "o" #'+pdf/toggle-outline
                                 :desc "PDF info" "i" #'+pdf/show-info
                                 :desc "Check status" "S" #'+pdf/check-tools-status
                                 :desc "Auto install" "I" #'+pdf/auto-install))))

(provide 'tools-pdf-config)
;;; tools-pdf-config.el ends here
