;;; yasnippet-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Enhanced Yasnippet setup with multiple snippet directories,
;; project-specific snippets, completion integration, and helper functions.
;; Features:
;; - Multiple snippet directories and smart organization
;; - Company integration for completion
;; - Ivy/Counsel interface for snippet selection
;; - Snippet creation helpers
;; - Support for different modes with appropriate hooks

;;; Code:

(use-package! yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)
         (markdown-mode . yas-minor-mode)
         (conf-mode . yas-minor-mode))
  :commands yas-minor-mode
  :custom
  (yas-snippet-dirs
   '("~/.config/emacs/snippets"              ;; personal snippets
     "~/.config/emacs/snippets/project-specific-snippets" ;; project-specific snippets
     "~/.local/share/yasnippet-snippets"))   ;; community snippets
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  (yas-prompt-functions '(yas-completing-prompt yas-ido-prompt yas-dropdown-prompt))
  (yas-verbosity 1)  ;; reduce verbosity but still show errors
  :config
  (yas-reload-all)

  ;; Auto expand common snippets in certain modes
  (defvar my/yas-auto-insert-snippet-condition-alist
    '((prog-mode . (lambda () nil))  ;; No auto for prog-mode
      (org-mode . (lambda ()
                    (when (looking-back "^\\s-*\\([*]+ \\|[0-9]+[.)] \\)" (line-beginning-position))
                      "org-item")))
      (markdown-mode . (lambda ()
                         (when (looking-back "^\\s-*\\([*-] \\)" (line-beginning-position))
                           "md-list-item"))))
    "Alist of major-mode and auto-insert conditions")

  ;; Add auto-expansion hook
  (add-hook 'post-self-insert-hook
            (defun my/yas-try-expanding-auto-snippets ()
              "Expand snippets when conditions are met."
              (when-let ((condition-func (alist-get major-mode my/yas-auto-insert-snippet-condition-alist)))
                (when-let ((snippet-name (funcall condition-func)))
                  (yas-expand-snippet (yas-lookup-snippet snippet-name major-mode))))))

  ;; Helper function to create new snippets quickly
  (defun my/yas-new-snippet ()
    "Create a new snippet in the appropriate directory."
    (interactive)
    (let* ((snippet-dirs (yas-snippet-dirs))
           (default-directory (if (> (length snippet-dirs) 0)
                                  (car snippet-dirs)
                                "~/.config/emacs/snippets")))
      (yas-new-snippet))))

;; Community snippets collection
(use-package! yasnippet-snippets
  :after yasnippet
  :config
  ;; Add any custom snippet category directories here if needed
  (yasnippet-snippets-initialize))

;; Ivy integration for yasnippet
(use-package! ivy-yasnippet
  :after (ivy yasnippet)
  :commands ivy-yasnippet
  :custom
  (ivy-yasnippet-expand-keys nil)  ;; Don't auto-expand keys
  :config
  ;; Show snippet content preview in ivy
  (setq ivy-yasnippet-preview t))

;; Company integration for yasnippet
(use-package! company-yasnippet
  :after (company yasnippet)
  :config
  (add-to-list 'company-backends #'company-yasnippet))

;; Project-specific snippets
(after! (projectile yasnippet)
  (defun my/load-project-snippets ()
    "Load project-specific snippets based on project type."
    (when-let* ((project-root (projectile-project-root))
                (project-type (projectile-project-type)))
      (let ((project-snippets-dir
             (expand-file-name
              (format "project-specific-snippets/%s" project-type)
              (car yas-snippet-dirs))))
        (when (file-directory-p project-snippets-dir)
          (add-to-list 'yas-snippet-dirs project-snippets-dir)
          (yas-reload-all)))))

  (add-hook! 'projectile-after-switch-project-hook #'my/load-project-snippets))

;; Keybindings for yasnippet
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("y" . "yasnippet")
                    :desc "Insert snippet"        "i" #'ivy-yasnippet
                    :desc "New snippet"           "n" #'my/yas-new-snippet
                    :desc "Visit snippet file"    "v" #'yas-visit-snippet-file
                    :desc "Find snippet"          "f" #'yas-find-snippets
                    :desc "Reload all snippets"   "r" #'yas-reload-all
                    :desc "Create snippet from region" "c" #'yas-new-snippet-with-content)))

;; Function to create a snippet from selected region
(defun yas-new-snippet-with-content ()
  "Create a new snippet with region content if active."
  (interactive)
  (if (region-active-p)
      (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
        (yas-new-snippet)
        (yas-expand-snippet (format "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}\n# --\n%s" content)))
    (call-interactively 'yas-new-snippet)))

(provide 'yasnippet-config)

;;; yasnippet-config.el ends here
