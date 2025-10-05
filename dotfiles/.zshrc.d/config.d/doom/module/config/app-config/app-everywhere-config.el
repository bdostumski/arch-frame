;;; app-everywhere-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Full configuration for Emacs server and everywhere editing in Doom Emacs.

;;; Code:

(use-package! server
  :defer 1  ; Load after 1 second to avoid startup conflicts
  :config
  ;; Start Emacs server if not already running (with error handling)
  (unless (server-running-p)
    (condition-case err
        (server-start)
      (file-already-exists
       (message "Server socket file already exists, attempting to delete and restart...")
       (server-force-delete)
       (server-start))
      (error
       (message "Failed to start Emacs server: %s" (error-message-string err))))))

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

;; Ask before killing Emacs if there are active clients
(setq confirm-kill-emacs 'yes-or-no-p)

;; Optimized server start message (runs only once, with delay)
(defvar +everywhere/server-message-shown nil
  "Flag to track if server message has been shown.")

(defun +everywhere/everywhere-server-start-message ()
  "Show server start message once."
  (unless +everywhere/server-message-shown
    (setq +everywhere/server-message-shown t)
    (run-with-timer 1.0 nil
                    (lambda ()
                      (message "Emacs server is running. Use 'emacsclient' to edit files!")))))

(add-hook 'server-after-make-frame-hook #'+everywhere/everywhere-server-start-message)


;; Optional: show a message when server starts
;;(defun app/everywhere-server-start-message ()
;;  (message "Emacs server is running. Use 'emacsclient' to edit files from the shell!"))
;;(add-hook 'server-after-make-frame-hook #'app/everywhere-server-start-message)

(provide 'app-everywhere-config)

;;; app-everywhere-config.el ends here
