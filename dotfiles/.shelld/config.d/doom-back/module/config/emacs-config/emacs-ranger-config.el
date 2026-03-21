
;;; module/config/emacs-config/ranger-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal Ranger configuration for manual invocation with Doom Emacs.
;; Provides a leader keybinding for quick access.

;;; Code:

(use-package! ranger
  :defer t
  :commands ranger ;; manually call with M-x ranger
  :config
  ;; === Core Settings ===
  (setq ranger-show-hidden t              ; Show hidden files by default
        ranger-cleanup-eagerly t          ; Clean up buffers more aggressively
        ranger-cleanup-on-disable t       ; Clean up when disabling ranger
        ranger-parent-depth 1             ; Show 2 levels of parent directories
        ranger-width-parents 0.15         ; Parent window width (15%)
        ranger-max-parent-width 0.25      ; Maximum parent width
        ranger-width-preview 0.55         ; Preview window width (55%)
        ranger-max-preview-size (* 10 1024 1024)) ; Max preview size (10MB)

  ;; === Preview Settings ===
  (setq ranger-show-literal t             ; Show literal preview for text files
        ranger-dont-show-binary t         ; Don't preview binary files
        ranger-modify-header t            ; Show custom header
        ranger-header-func 'ranger-header-line) ; Custom header function

  ;; === Navigation Settings ===
  (setq ranger-deer-show-details nil        ; Show file details in deer mode
        ranger-return-to-ranger t         ; Return to ranger after file operations
        ranger-persistent-sort t          ; Remember sort settings
        ranger-listing-switches "-alh")   ; Default ls switches

  ;; === Integration Settings ===
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4" "avi" "mov" "wmv") ; Don't preview these
        ranger-max-preview-size (* 5 1024 1024)) ; 5MB preview limit

  ;; === File Type Associations ===
  (setq ranger-ignored-extensions
        '("mkv" "webm" "mp4" "avi" "wmv" "mov" "flv" "ogv"
          "mp3" "flac" "wav" "aac" "ogg" "wma"
          "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx"
          "zip" "rar" "7z" "tar" "gz" "bz2" "xz"
          "exe" "msi" "deb" "rpm" "dmg" "pkg"
          "jpg" "jpeg" "png" "gif" "bmp" "tiff" "webp"
          "iso" "img" "bin" "nrg"))

  (defun ranger-open-in-external-app ()
    "Open current file in external application."
    (interactive)
    (when-let ((file (ranger-get-current-file)))
      (cond
       ((eq system-type 'darwin)
        (shell-command (format "open %s" (shell-quote-argument file))))
       ((eq system-type 'gnu/linux)
        (shell-command (format "xdg-open %s" (shell-quote-argument file))))
       ((eq system-type 'windows-nt)
        (shell-command (format "start %s" (shell-quote-argument file)))))
      (message "Opening %s externally" (file-name-nondirectory file))))


  ;; === Performance Optimizations ===
  (setq ranger-max-files-size 5000) ; Don't show directories with more than 5000 files

  ;; === Custom Header Function ===
  (defun ranger-header-line ()
    "Custom header line for ranger."
    (let ((path (abbreviate-file-name default-directory)))
      (format " %s [%d files]"
              path
              (length (directory-files default-directory)))))

  (defun ranger-size-human-readable (file)
    "Return human-readable size for FILE."
    (let ((size (file-attribute-size (file-attributes file))))
      (if size
          (file-size-human-readable size)
        "")))


  ;; === Additional Hooks ===
  (add-hook 'ranger-mode-hook
            (lambda ()
              ;; Hide cursor in parent and preview windows
              (setq cursor-type nil)
              ;; Disable line numbers in ranger
              (display-line-numbers-mode -1)
              ;; Disable visual line mode
              (visual-line-mode -1)))


  )

(provide 'file-managers-config)

;;; ranger-config.el ends here
