;;; module/config/completion-config/completion-ivy-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Ivy-based completion setup with rich UI enhancements, icons, sorting/persisting,
;; posframe support, and additional productivity features for a modern Emacs experience.

;;; Code:

;; ----------------------------
;; Ivy: core incremental completion
;; ----------------------------
(use-package! ivy
  :diminish
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-c C-r" . ivy-resume))
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)           ;; include recent files/buffers
  (enable-recursive-minibuffers t)      ;; allow recursive minibuffers
  (ivy-count-format "(%d/%d) ")         ;; show match count
  (ivy-height 15)                       ;; increase minibuffer height
  (ivy-wrap t)                          ;; wrap around results
  (ivy-use-selectable-prompt t)         ;; make prompt selectable
  (ivy-fixed-height-minibuffer t)       ;; consistent height
  (ivy-initial-inputs-alist nil)        ;; remove default ^ from searches
  (ivy-re-builders-alist                ;; configure regex builders
   '((ivy-switch-buffer . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  :config
  ;; Custom actions for different contexts
  (ivy-set-actions
   'counsel-find-file
   '(("j" find-file-other-window "other window")
     ("f" find-file-other-frame "other frame")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("x" counsel-find-file-extern "open externally")
     ("d" delete-file "delete")
     ("r" counsel-find-file-as-root "open as root"))))

;; ----------------------------
;; Counsel: Ivy-enhanced commands
;; ----------------------------
(use-package! counsel
  :after ivy
  :diminish
  :bind (("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :custom
  (counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; ----------------------------
;; Swiper: enhanced search
;; ----------------------------
(use-package! swiper
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward))
  :custom
  (swiper-action-recenter t)
  (swiper-include-line-number-in-search t))

;; ----------------------------
;; Ivy-Rich: show extra info in completions
;; ----------------------------
(use-package! ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1)
  :custom
  (ivy-rich-path-style 'abbrev)
  (ivy-rich-parse-remote-buffer t)
  :config
  ;; Custom transformers for better display
  (ivy-rich-set-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-icon (:width 2))
     (ivy-rich-candidate (:width 0.3))
     (ivy-rich-switch-buffer-size (:width 7))
     (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
     (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
     (ivy-rich-switch-buffer-project (:width 15 :face success))
     (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))))
  
  (ivy-rich-set-columns
   'counsel-M-x
   '((counsel-M-x-transformer (:width 40))
     (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
  
  (ivy-rich-set-columns
   'counsel-find-file
   '((ivy-rich-file-icon (:width 2))
     (ivy-rich-candidate (:width 0.8))
     (ivy-rich-file-size (:width 8))
     (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))

;; ----------------------------
;; All-the-icons integration for Ivy
;; ----------------------------
(use-package! all-the-icons-ivy-rich
  :after (ivy-rich all-the-icons)
  :init
  (all-the-icons-ivy-rich-mode 1)
  :config
  (all-the-icons-ivy-rich-reload))

;; ----------------------------
;; Prescient: smarter sorting and filtering
;; ----------------------------
(use-package! ivy-prescient
  :after ivy
  :custom
  (ivy-prescient-retain-classic-highlighting t)
  (prescient-history-length 1000)
  :init
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; ----------------------------
;; Posframe: popup completion UI
;; ----------------------------
(use-package! ivy-posframe
  :after ivy
  :diminish
  :custom
  (ivy-posframe-width 120)
  (ivy-posframe-min-width 80)
  (ivy-posframe-height 20)
  (ivy-posframe-min-height 10)
  (ivy-posframe-border-width 1)
  (ivy-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)
     (internal-border-width . 1)))
  (ivy-posframe-display-functions-alist
   '((swiper . ivy-posframe-display-at-point)
     (swiper-isearch . ivy-posframe-display-at-point)
     (counsel-M-x . ivy-posframe-display-at-frame-center)
     (counsel-find-file . ivy-posframe-display-at-frame-center)
     (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
     (t . ivy-posframe-display-at-frame-center)))
  :hook
  (after-init . ivy-posframe-mode)
  :config
  ;; Custom face for posframe
  (set-face-attribute 'ivy-posframe nil :background (face-background 'default))
  (set-face-attribute 'ivy-posframe-border nil :background (face-foreground 'default)))

;; ----------------------------
;; Ivy-Hydra: additional actions
;; ----------------------------
(use-package! ivy-hydra
  :after ivy
  :bind (:map ivy-minibuffer-map
              ("C-o" . ivy-dispatching-done)
              ("M-o" . ivy-hydra/body)))

;; ----------------------------
;; Ivy-Yasnippet: snippet integration
;; ----------------------------
(use-package! ivy-yasnippet
  :after (ivy yasnippet)
  :bind ("C-c y" . ivy-yasnippet))

;; ----------------------------
;; Custom functions and enhancements
;; ----------------------------
(defun my/ivy-switch-project-buffer ()
  "Switch to buffer in current project."
  (interactive)
  (if (projectile-project-p)
      (ivy-read "Switch to project buffer: "
                (mapcar #'buffer-name (projectile-project-buffers))
                :action #'switch-to-buffer)
    (ivy-switch-buffer)))

(defun my/ivy-recentf-and-bookmarks ()
  "Combine recentf and bookmarks in ivy."
  (interactive)
  (ivy-read "Recent files and bookmarks: "
            (append (mapcar 'abbreviate-file-name recentf-list)
                    (bookmark-all-names))
            :action (lambda (x)
                      (if (file-exists-p x)
                          (find-file x)
                        (bookmark-jump x)))))

;; ----------------------------
;; Performance optimizations
;; ----------------------------
(setq ivy-dynamic-exhibit-delay-ms 200)  ;; reduce delay for better performance
(setq ivy-use-virtual-buffers t)
(setq ivy-virtual-abbreviate 'fullpath)

;; ----------------------------
;; Key bindings
;; ----------------------------
(global-set-key (kbd "C-c r") #'my/ivy-recentf-and-bookmarks)
(global-set-key (kbd "C-c b") #'my/ivy-switch-project-buffer)

;; ----------------------------
;; Integration with other packages
;; ----------------------------
(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy))

(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'completion-ivy-config)

;;; completion-ivy-config.el ends here
