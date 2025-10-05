;;; app-emms-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full EMMS (Emacs Multimedia System) configuration for Doom Emacs.

;;; Code:

(use-package! emms
  :defer t
  :init
  ;; Set default music library directory
  (setq emms-source-file-default-directory "~/Music/")
  :config
  ;; Load main EMMS modules
  (emms-all)
  (emms-default-players)

  ;; Info backends (for metadata, album art, etc.)
  (setq emms-info-functions '(emms-info-libtag emms-info-metaflac emms-info-ogginfo))

  ;; Playlist and browser settings
  (setq emms-playlist-buffer-name "*Music*"
        emms-browser-covers 'emms-browser-cache-thumbnail
        emms-browser-info-async t)

  ;; Only set default covers if the file exists
  (let ((default-cover (expand-file-name "emms-browser-default-cover.png" doom-private-dir)))
    (when (file-exists-p default-cover)
      (setq emms-browser-default-covers (list default-cover))))

  ;; Track description in mode-line
  (setq emms-mode-line-format " [%s]"
        emms-playing-time-display-format " [%s]")

  (emms-mode-line 1)
  (emms-playing-time 1)

  ;; Volume control (FIXED: removed infinite recursion)
  (defun app/emms-volume-raise ()
    "Raise EMMS volume by 5."
    (interactive)
    (emms-volume-change "+5"))

  (defun app/emms-volume-lower ()
    "Lower EMMS volume by 5."
    (interactive)
    (emms-volume-change "-5"))

  ;; Smart track info display
  (setq emms-track-description-function
        (lambda (track)
          (let ((artist (emms-track-get track 'info-artist))
                (title (emms-track-get track 'info-title)))
            (if (and artist title)
                (format "%s - %s" artist title)
              (emms-track-simple-description track)))))

  ;; Playlist settings
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-hook 'emms-playlist-mode-hook #'read-only-mode))

(provide 'app-emms-config)

;;; app-emms-config.el ends here
