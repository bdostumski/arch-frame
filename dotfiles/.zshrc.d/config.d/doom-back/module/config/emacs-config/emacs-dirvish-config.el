;;; module/config/emacs-config/emacs-dirvish-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Optimized Dirvish configuration with quick access entries and preview dispatchers.

;;; Code:

(use-package! dirvish
  :after dired
  :init

  ;; Override standard Dired with Dirvish
  (dirvish-override-dired-mode)

  :custom
  ;; Quick access shortcuts
  ;; Quick access shortcuts - expanded with more useful locations
  (call-interactively #'dirvish-quick-access)
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Documents/" "Documents")
     ("w" "~/Workspace/" "Workspace")
     ("D" "~/Downloads/" "Downloads")
     ("p" "~/Pictures/" "Pictures")
     ("v" "~/Videos/" "Videos")
     ("m" "~/Music/" "Music")
     ("c" "~/.config/" "Config")
     ("e" "~/.emacs.d/" "Emacs Config")))

  ;; File preview handlers
  (dirvish-preview-dispatchers
   '(image gif video audio epub pdf archive))

  ;; === Advanced Display Configuration ===

  ;; Header line format - show useful information
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))

  ;; Mode line format - customize what's shown in mode line
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))

  ;; Attributes to show in file listings
  ;; Additional: subtree-state
  (dirvish-attributes
   '(all-the-icons file-time file-size collapse vc-state git-msg ))

  ;; === File Operations ===

  ;; Use rsync for remote file operations when available
  (dirvish-use-header-line t)
  (dirvish-default-layout '(0 0.4 0.6))

  ;; Cache settings for better performance
  (dirvish-cache-dir (expand-file-name "dirvish/" doom-cache-dir))

  ;; === Preview Configuration ===

  ;; Image preview limiter settings
  ;;(dirvish-image-preview-size 512)

  ;; Video preview limiter settings
  ;;(dirvish-video-preview-threshold (* 50 1024 1024)) ; 50MB

  ;; Archive preview settings
  (dirvish-archive-bindings
   '(("7z" . "7z l")
     ("zip" . "unzip -l")
     ("tar" . "tar -tf")
     ("gz" . "gzip -l")
     ("bz2" . "bzip2 -l")))

  ;; === Sorting and Filtering ===

  ;; Default sorting method
  (dired-listing-switches "-laGh1v --group-directories-first")

  ;; Hide dotfiles by default (toggle with 'H')
  ;;(dired-omit-files "^\\.[^.]")

  ;; === Hooks ===

  ;; Auto-refresh when files change
  (add-hook 'dirvish-find-entry-hook #'revert-buffer)

  ;; Clean up cache periodically
  (run-with-timer 3600 3600 ; Every hour
                  (lambda ()
                    (when (file-directory-p dirvish-cache-dir)
                      (let ((old-files (directory-files-recursively
                                        dirvish-cache-dir ".*" nil
                                        (lambda (file)
                                          (time-less-p (days-to-time 7)
                                                       (time-since (file-attribute-modification-time
                                                                    (file-attributes file))))))))
                        (dolist (file old-files)
                          (delete-file file))))))

  ;; === Conditional Features ===

  ;; Git integration
  (when (featurep 'magit)
    (define-key dirvish-mode-map (kbd "M-g") #'magit-status))

  ;; Projectile integration
  (when (featurep 'projectile)
    (define-key dirvish-mode-map (kbd "M-p") #'projectile-find-file))

  ;; Treemacs integration
  (when (featurep 'treemacs)
    (define-key dirvish-mode-map (kbd "M-t") #'treemacs-select-window)))

(provide 'emacs-dirvish-config)

;;; emacs-dirvish-config.el ends here
