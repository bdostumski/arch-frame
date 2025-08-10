;; ============================
;; 🔧 Main Configuration
;; ============================

(setq user-full-name "Borislav Dostumski"
      user-mail-address "b.dostumski@gmail.com")

(setq doom-theme 'doom-wilmersdorf)
(add-to-list 'default-frame-alist '(undecorated . t))
(solaire-global-mode +1)

(setq use-package-always-ensure t)

(setq projectile-auto-discover t)

(use-package! beacon
  :config
  (beacon-mode 1))

(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package! git-link
  :commands git-link)

(use-package! git-messenger
  :commands git-messenger:popup-message)

(use-package! sudo-edit
  :commands sudo-edit)

(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode)

;; ============================
;; 📡 Code Intelligence & LSP
;; ============================

(use-package! lsp-mode
  :hook ((java-mode . lsp)
         (js-mode . lsp)
         (python-mode . lsp))
  :commands lsp)

(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t))

(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package! lsp-treemacs
  :after lsp)

(use-package! gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))

(use-package! maven-test-mode
  :commands maven-test-mode)

(use-package! flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================
;; 🔤 Completion & Snippets
;; ============================

(use-package! company
  :hook (after-init . global-company-mode))

(use-package! company-box
  :hook (company-mode . company-box-mode))

(use-package! ivy
  :config
  (ivy-mode 1))

(use-package! ivy-posframe
  :after ivy
  :config
  (ivy-posframe-mode 1))

(use-package! ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package! ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))

(use-package! all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package! yasnippet-snippets
  :after yasnippet)

(use-package! emmet-mode
  :hook ((html-mode css-mode web-mode) . emmet-mode))

(use-package! fish-mode
  :mode "\\.fish\\'")

(use-package! bash-completion
  :after shell)

(use-package! jq-mode
  :mode "\\.jq\\'")

(use-package! prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

;; ============================
;; 🎨 UI Enhancements
;; ============================

(use-package! solaire-mode
  :config
  (solaire-global-mode +1))

(use-package! ace-window
  :bind ("M-o" . ace-window))

(use-package! avy
  :bind ("C-:" . avy-goto-char))

(use-package! expand-region
  :bind ("C-=" . er/expand-region))

(use-package! deadgrep
  :commands deadgrep)

(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package! smooth-scrolling
  :config (smooth-scrolling-mode 1))

(use-package! minimap
  :commands minimap-mode)

(use-package! visual-fill-column
  :hook (text-mode . visual-fill-column-mode))

(use-package! tree-sitter
  :hook (prog-mode . tree-sitter-mode))

(use-package! tree-sitter-langs
  :after tree-sitter)

(use-package! treemacs
  :defer t)

(use-package! treemacs-all-the-icons)
(use-package! treemacs-icons-dired)
(use-package! treemacs-magit)
(use-package! treemacs-persp)
(use-package! treemacs-projectile)
(use-package! treemacs-tab-bar)

;; ============================
;;📒 Writing, Org & Notes
;; ============================

(use-package! deft
  :config
  (setq deft-directory "~/notes"
        deft-extensions '("org" "md" "txt")
        deft-recursive t))

(use-package! emojify
  :hook (after-init . global-emojify-mode))

(use-package! org-appear
  :hook (org-mode . org-appear-mode))

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-screenshot-method "flameshot gui"))

;;(use-package! org-gcal
;;  :config
;;  :init
;;  (setq org-gcal-client-id "your-client-id"
;;        org-gcal-client-secret "your-client-secret"
;;        org-gcal-file-alist '(("your-email@gmail.com" .  "~/org/gcal.org"))))

(use-package! org-modern
  :hook (org-mode . org-modern-mode))

(use-package! org-roam-ui
  :after org-roam)

(use-package! org-super-agenda
  :config
  (org-super-agenda-mode))

(use-package! ox-hugo
  :after ox)

;; LaTeX / Math
(use-package! auctex-latexmk)
(use-package! math-preview)
(use-package! cdlatex)

;; ============================
;; 🧪 Testing & Dev Tools
;; ============================

(use-package! cider
  :hook (clojure-mode . cider-mode))

(use-package! clojure-mode
  :mode "\\.clj\\'")

(use-package! ejc-sql
  :commands ejc-sql-mode)

(use-package! graphql-mode
  :mode "\\.graphql\\'")

(use-package! test-simple
  :commands test-simple-run)

;; ============================
;; 🤖 AI / Copilot / LLM
;; ============================

(after! copilot
  ;; Enable copilot in programming modes
  (add-hook 'prog-mode-hook #'copilot-mode)

  ;; Optional: adjust idle delay
  (setq copilot-idle-delay 0.5))


;; ============================
;;📒 Mail Client
;; ============================

(require 'mu4e)

;; Use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX"              :key ?i)
         (:maildir "/[Gmail].Sent Mail"  :key ?s)
         (:maildir "/[Gmail].Trash"      :key ?t)
         (:maildir "/[Gmail].All Mail"   :key ?a)))

(add-to-list 'mu4e-bookmarks
             ;; ':favorite t' i.e, use this one for the modeline
             '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "b.dostumski@gmail.com"
 user-full-name  "Borislav Dostumski"
 message-signature
 (concat
  "Borislav Dostumski\n"
  "http://www.github.com/bdostumski\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
;;(setq message-send-mail-function 'smtpmail-send-it
;;   starttls-use-gnutls t
;;   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;   smtpmail-auth-credentials
;;     '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;;   smtpmail-default-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-default-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-service 587)

(setq sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)

;; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Mu4e mail directory
(setq mu4e-maildir "~/Maildir")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; ============================
;; 🧩 Optional / Disabled
;; ============================

