;;; app-everywhere-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full configuration for Emacs server and everywhere editing in Doom Emacs.
;; - Starts Emacs server automatically if not running
;; - Provides leader keybindings for emacsclient functions and server management

;;; Code:

(after! server
  ;; Start Emacs server if not already running
  (unless (server-running-p)
    (server-start)))

;; Optionally, you can enable server-mode everywhere
;; (add-hook 'after-init-hook #'server-mode)

;; ----------------------------
;; Auto-save and auto-kill behavior for emacsclient
;; ----------------------------

;; Automatically save and kill the buffer after server-edit
(setq server-window 'pop-to-buffer
      server-switch-hook nil
      server-kill-buffer-on-quit t)

;; Optional: confirmation before killing buffers from emacsclient
;; (setq server-kill-buffer-query nil)

;; Optional: ask before killing Emacs if there are active clients
(setq confirm-kill-emacs 'yes-or-no-p)

;; Optional: show a message when server starts
(defun app/everywhere-server-start-message ()
  (message "Emacs server is running. Use 'emacsclient' to edit files from the shell!"))
(add-hook 'server-after-make-frame-hook #'app/everywhere-server-start-message)

;; ----------------------------
;; Leader keybindings
;; ----------------------------
;;(map! :leader
;;      (:prefix-map ("e" . "everywhere")
;;       :desc "Edit file with emacsclient (server-edit)" "f" #'server-edit
;;       :desc "Start server"  "s" #'server-start
;;       :desc "Stop server"   "k" #'server-force-delete
;;       :desc "Show server running status" "r" (lambda () (interactive)
;;                                                (message "Server running: %s" (server-running-p)))))

(provide 'app-everywhere-config)

;;; app-everywhere-config.el ends here
