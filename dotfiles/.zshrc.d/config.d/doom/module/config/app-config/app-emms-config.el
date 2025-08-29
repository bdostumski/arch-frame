;;; config/app-config/app-emms-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; EMMS (Emacs Multimedia System) configuration.
;; - Initializes EMMS player and default players
;; - Sets default music directory
;; - Provides convenient leader keybindings for playback control

;;; Code:

(after! emms
  ;; Initialize EMMS
  (emms-all)
  (emms-default-players)

  ;; Set default music library directory
  (setq emms-source-file-default-directory "~/Music/"))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("m" . "music")
;;       :desc "Play/Pause"       "p" #'emms-pause
;;       :desc "Next track"       "n" #'emms-next
;;       :desc "Previous track"   "b" #'emms-previous
;;       :desc "Open playlist"    "l" #'emms-playlist-mode-go))

(provide 'app-emms-config)

;;; app-emms-config.el ends here
