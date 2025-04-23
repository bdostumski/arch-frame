;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; UI/UX Add-ons
(setq doom-theme 'doom-one
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16)
      display-line-numbers-type 'relative)

(map! :leader
      :desc "Project sidebar" "t p" #'treemacs)

;; Project & File Management
(setq projectile-project-search-path '("~/Workspace/" "~/Documents/"))

;; Terminal & Shells
(setq shell-file-name "/bin/zsh") ;; or bash/fish

;; Git Integration
(map! :leader
      :desc "Magit status" "g s" #'magit-status)

;; Install forge
(after! magit
  (require 'forge))

;; Database Tools
;; Use ejc-sql or sqlup-mode for advanced DB work.
(setq sql-connection-alist
      '((local-pg
         (sql-product 'postgres)
         (sql-user "postgres")
         (sql-database "mydb")
         (sql-server "localhost")
         (sql-port 5432))))

;; Notes, Org-mode, and Docs
(after! org
  (setq org-directory "~/Documents/org/"
        org-log-done 'time))

(use-package! org-roam
  :init
  (setq org-roam-directory "~/Documents/org/roam")
  :config
  (org-roam-db-autosync-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Auto completion +LSP Performance
(setq company-idle-delay 0.1
      company-minimum-prefix-length 1)

(setq lsp-enable-snippet t
      lsp-headerline-breadcrumb-enable t
      lsp-idle-delay 0.5
      lsp-log-io nil
      lsp-ui-sideline-enable t
      lsp-ui-doc-enable t)

(after! lsp-mode
  (setq lsp-inlay-hint-enable t
        lsp-completion-provider :capf))

(after! lsp-mode
  (setq lsp-enable-symbol-highlighting t
        lsp-modeline-diagnostics-enable t))

(add-hook 'prog-mode-hook #'company-mode)

(use-package! org-roam-ui
  :after org-roam
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t))
;; org-roam-ui-update-on-save t

(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; Rest Client Config
(use-package! restclient
  :mode ("\\.http\\'" . restclient-mode))
(use-package! company-restclient
  :after restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

;; Docker Integration
(use-package! docker
  :commands (docker)
  :config
  (setq docker-run-as-root nil))

(use-package! dockerfile-mode
  :mode "Dockerfile\\'")

;; Kubernetes Configuration
(use-package! kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600))

(use-package! kubernetes-evil
  :after kubernetes)

;; Mail Configuration
(setq mu4e-get-mail-command "mbsync gmail"
      mu4e-update-interval 1800
      mu4e-maildir "~/.mail/gmail"
      mu4e-compose-format-flowed t)

(set-email-account!
 "Gmail"
 '((mu4e-sent-folder       . "/gmail/[Gmail]/Sent Mail")
   (mu4e-drafts-folder     . "/gmail/[Gmail]/Drafts")
   (mu4e-trash-folder      . "/gmail/[Gmail]/Trash")
   (mu4e-refile-folder     . "/gmail/[Gmail]/All Mail")
   (smtpmail-smtp-user     . "your@gmail.com")
   (smtpmail-smtp-server   . "smtp.gmail.com")
   (smtpmail-smtp-service  . 587)
   (smtpmail-stream-type   . starttls)
   (user-mail-address      . "your@gmail.com")
   (user-full-name         . "Your Name")
   (mu4e-compose-signature . "---\nYour Name"))
 t)

;; Direnv Configuration
(use-package! envrc
  :config
  (envrc-global-mode))

;; COPILOT and LSP
;; Enable Copilot in programming modes
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  ;; Keybinding to accept Copilot suggestions
  (define-key copilot-mode-map (kbd "C-<return>") 'copilot-accept-completion)

  ;; Configure Copilot-specific settings
  (setq copilot-completion-delay 0.5)  ;; Delay Copilot suggestions to avoid being too aggressive
  (setq copilot-enable-prompting t)    ;; Allow Copilot to prompt for suggestions
  (defun copilot--infer-indentation-offset ()
    (or tab-width 4))  ;; Set an indentation offset for Copilot

  ;; Disable Copilot by default
  (setq copilot-mode nil))  ;; Keep Copilot off unless explicitly enabled

;; Enable Company in programming modes
(use-package! company
  :hook (prog-mode . company-mode)
  :config
  ;; Keybinding to trigger Company completion manually
  (global-set-key (kbd "C-M-i") 'company-complete)  ;; Manually trigger completion

  ;; Set Company-specific configurations
  (setq company-idle-delay 0.2)  ;; Show suggestions after a short delay
  (setq company-minimum-prefix-length 2)  ;; Trigger suggestions after typing 2 characters
  (setq company-backends '((company-capf company-dabbrev-code company-files company-yasnippet))))  ;; Include Capf and other backends

;; Define the toggle function for Copilot and Company suggestions
(defun toggle-copilot-and-company ()
  "Toggle Copilot and Company suggestions."
  (interactive)
  (if copilot-mode
      (progn
        (copilot-mode -1)
        (add-to-list 'company-backends 'company-capf)) ;; Enable Company when Copilot is off
    (progn
      (copilot-mode 1)
      (setq company-backends
            (delete 'company-capf company-backends))))) ;; Remove Company-capf backend when Copilot is on

(global-set-key (kbd "C-S-C") 'toggle-copilot-and-company) ;; Bind the toggle function to a key combination   

;; Additional configuration for indentation (for Copilot)
(setq-default tab-width 4)
(setq-default standard-indent 4)

;; Enable both Copilot and Company completion (using their respective backends)
(after! company
  (add-to-list 'company-backends 'company-capf))  ;; Add company-capf as backend for Company

;; ORUI
;;(use-package! orui
;;  :hook (org-mode . orui-mode))

;; Ein - Jupyter
(use-package! ein
  :commands (ein:notebooklist-open))

;; Grip (markdown preview)
(use-package! grip-mode
  :hook (markdown-mode . grip-mode) ;; grip-mode only when needed
  :config
  (setq grip-update-after-change nil
        grip-github-user "your-username"
        grip-github-password "your-token")) ;; replace or use authinfo.gpg

;; Circe (IRC)
(setq circe-default-nick "doomdev"
      circe-default-user "doomdev"
      circe-network-options
      '(("Freenode" :tls t :nick "doomdev" :sasl-username "doomdev" :sasl-password "yourpass")))

;; Ement (Matrix)
(use-package! ement
  :commands (ement-connect))

;; RSS Feeds
(setq elfeed-feeds
      '("https://hnrss.org/frontpage"
        "https://lobste.rs/rss"
        "https://www.reddit.com/r/linux/.rss"
        "https://xkcd.com/rss.xml"))
