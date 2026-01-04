;;; app-irc-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; IRC client setup for Doom Emacs.

;;; Code:

(use-package! rcirc
  :defer t
  :init
  ;; Enhanced server configuration with environment variable support
  (setq rcirc-server-alist
        `(("irc.libera.chat"
           :port 6697
           :encryption tls
           :nick ,(or (getenv "IRC_NICK") "your-nick")
           :user-name ,(or (getenv "IRC_USERNAME") "your-username")
           :full-name ,(or (getenv "IRC_FULLNAME") "Your Name")
           :password ,(getenv "IRC_PASSWORD"))))  ; For NickServ authentication

  ;; Channel configuration with server-specific settings
  (setq rcirc-default-chan-alist
        '(("irc.libera.chat" "#emacs" "#doom-emacs")))

  ;; Logging configuration
  (setq rcirc-log-directory (expand-file-name "rcirc-logs/" doom-cache-dir)
        rcirc-log-flag t
        rcirc-log-directory-per-server t)

  ;; Performance and UI improvements
  (setq rcirc-prompt "➤ "
        rcirc-fill-column (- (window-width) 2)
        rcirc-buffer-maximum-lines 2000  ; Limit buffer size for performance
        rcirc-decode-coding-system 'utf-8
        rcirc-encode-coding-system 'utf-8
        rcirc-time-format "%H:%M "
        rcirc-timeout-seconds 60
        rcirc-reconnect-delay 5)

  ;; Better connection handling
  (setq rcirc-auto-authenticate-flag t
        rcirc-authenticate-before-join t)

  :config
  ;; Ensure log directory exists
  (unless (file-directory-p rcirc-log-directory)
    (make-directory rcirc-log-directory t))

  ;; Enhanced logging with automatic cleanup
  (defun rcirc-write-log-enhanced (process sender response target text)
    "Enhanced logging function with better formatting and cleanup."
    (when (and rcirc-log-flag target)
      (let* ((filename (rcirc-generate-log-filename process target))
             (cell (assoc-string filename rcirc-log-alist))
             (buffer (if cell
                         (cdr cell)
                       (setq cell (cons filename (generate-new-buffer
                                                  " *rcirc-log*")))
                       (push cell rcirc-log-alist)
                       (cdr cell))))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
          (insert (format "<%s> %s\n"
                          (or sender "")
                          (rcirc-markup-text-functions text)))
          ;; Periodically save and clean up large buffers
          (when (> (buffer-size) 100000)
            (write-region (point-min) (point-max) filename t 'quiet)
            (erase-buffer))))))

  ;; Performance optimizations
  (add-hook 'rcirc-mode-hook
            (lambda ()
              (rcirc-omit-mode 1)
              (setq-local scroll-conservatively 8192)
              (setq-local auto-window-vscroll nil)))

  ;; Enhanced print hooks
  (remove-hook 'rcirc-print-hooks 'rcirc-print-log)  ; Remove default logging
  (add-hook 'rcirc-print-hooks 'rcirc-write-log-enhanced)

  ;; Notification support (if available)
  (when (featurep 'alert)
    (defun rcirc-notify (process sender response target text)
      "Send desktop notification for mentions."
      (when (and (string-match (rcirc-nick process) text)
                 (not (string= (rcirc-nick process) sender)))
        (alert text
               :title (format "IRC: %s in %s" sender target)
               :category 'rcirc)))
    (add-hook 'rcirc-print-hooks 'rcirc-notify))

  ;; Auto-reconnection with exponential backoff
  (defvar rcirc-reconnect-attempts 0
    "Number of reconnection attempts.")

  (defun rcirc-auto-reconnect ()
    "Automatically reconnect with exponential backoff."
    (when (< rcirc-reconnect-attempts 5)
      (let ((delay (* rcirc-reconnect-delay (expt 2 rcirc-reconnect-attempts))))
        (message "Attempting to reconnect in %d seconds..." delay)
        (run-at-time delay nil
                     (lambda ()
                       (condition-case err
                           (progn
                             (rcirc-reconnect-all)
                             (setq rcirc-reconnect-attempts 0))
                         (error
                          (setq rcirc-reconnect-attempts (1+ rcirc-reconnect-attempts))
                          (message "Reconnection failed: %s" err))))))))

  (add-hook 'rcirc-sentinel-hooks 'rcirc-auto-reconnect)

  ;; Log cleanup function
  (defun rcirc-cleanup-old-logs ()
    "Clean up log files older than 30 days."
    (interactive)
    (when (file-directory-p rcirc-log-directory)
      (dolist (file (directory-files rcirc-log-directory t "\\.log$"))
        (when (> (time-to-seconds (time-since (nth 5 (file-attributes file))))
                 (* 30 24 60 60))  ; 30 days
          (delete-file file)
          (message "Deleted old log file: %s" file)))))

  ;; Run cleanup weekly
  (run-at-time "1 week" (* 7 24 60 60) 'rcirc-cleanup-old-logs))

(provide 'app-irc-config)

;;; app-irc-config.el ends here
