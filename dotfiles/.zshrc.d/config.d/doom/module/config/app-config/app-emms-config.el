;;; app-emms-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full EMMS (Emacs Multimedia System) configuration for Doom Emacs.
;; - Initializes EMMS, default players, and info backends
;; - Sets default music directory and playlists
;; - Enables metadata, volume, and playback features
;; - Displays track info in mode line
;; - Provides convenient leader keybindings for playback and control

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
        emms-browser-info-async t
        emms-browser-default-covers
        (list (expand-file-name "emms-browser-default-cover.png" doom-private-dir)))

  ;; Track description in mode-line
  (setq emms-mode-line-format " [%s]"
        emms-playing-time-display-format " [%s]")

  (emms-mode-line 1)
  (emms-playing-time 1)

  ;; Volume control (requires amixer or similar for ALSA)
  (defun emms-volume-raise () (interactive) (emms-volume-raise 5))
  (defun emms-volume-lower () (interactive) (emms-volume-lower 5))

  ;; Smart track info display
  (setq emms-track-description-function
        (lambda (track)
          (let ((artist (emms-track-get track 'info-artist))
                (title (emms-track-get track 'info-title)))
            (if (and artist title)
                (format "%s - %s" artist title)
              (emms-track-simple-description track)))))

  ;; Keybindings in playlist mode
  (map!
   :map emms-playlist-mode-map
   "RET" #'emms-playlist-mode-play-smart
   "d" #'emms-playlist-mode-kill-track
   "u" #'emms-playlist-mode-undo
   "s" #'emms-shuffle)

  ;; Optionally, auto-save playlist
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-hook 'emms-playlist-mode-hook #'read-only-mode)
  )

;; ----------------------------
;; Leader keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("m" . "emms")
                                 :desc "Play/Pause"         "p" #'emms-pause
                                 :desc "Play file/directory""f" #'emms-play-directory-tree
                                 :desc "Next track"         "n" #'emms-next
                                 :desc "Previous track"     "b" #'emms-previous
                                 :desc "Stop"               "s" #'emms-stop
                                 :desc "Show playlist"      "l" #'emms-playlist-mode-go
                                 :desc "Show browser"       "B" #'emms-browser
                                 :desc "Add file"           "a" #'emms-add-file
                                 :desc "Add directory"      "d" #'emms-add-directory
                                 :desc "Add playlist"       "P" #'emms-add-playlist
                                 :desc "Shuffle playlist"   "S" #'emms-shuffle
                                 :desc "Show current info"  "i" #'emms-show
                                 :desc "Seek forward"       ">" #'emms-seek-forward
                                 :desc "Seek backward"      "<" #'emms-seek-backward
                                 :desc "Volume up"          "+" #'emms-volume-raise
                                 :desc "Volume down"        "-" #'emms-volume-lower))))

(provide 'app-emms-config)

;;; app-emms-config.el ends here
