;;; module/keybindings.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module is dedicated to all personal and custom keybindings for Doom Emacs.
;;
;; Usage:
;; 1. Define custom keybindings that reference your functions, commands, or macros.
;; 2. Ensure that any functions used in keybindings are loaded **before** this file
;;    (e.g., via a `personal-functions.el` module).
;; 3. Keep this file focused solely on keybindings; separate functions should reside
;;    in another module/file for clarity and maintainability.

;;; Code:

;; -------------------------------------------------------------------
;; Calendar Keybindings 
;; -------------------------------------------------------------------
(map! :leader
      (:prefix ("o" . "open")
       :desc "Calendar" "c" #'calendar))

(map! :leader
      (:prefix-map ("n" . "notes")
                   (:prefix-map ("c" . "calendar")
                    :desc "Open calendar" "c" #'calendar
                    :desc "Enable appointments" "a" #'+calendar/enable-appointments
                    :desc "Disable appointments" "d" #'+calendar/disable-appointments
                    :desc "Toggle org diary" "o" #'+calendar/toggle-org-diary-integration)))

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("c" . "calendar")
                                 :desc "Open calendar" "c" #'calendar
                                 :desc "Enable appointments" "a" #'+calendar/enable-appointments
                                 :desc "Disable appointments" "d" #'+calendar/disable-appointments
                                 :desc "Toggle org diary" "o" #'+calendar/toggle-org-diary-integration))))

;; -------------------------------------------------------------------
;; Diary Calendar Keybindings 
;; -------------------------------------------------------------------
(map! :leader
      (:prefix-map ("n" . "notes")
                   (:prefix-map ("D" . "diary")
                    :desc "Show diary entries" "s" #'+diary/diary-show-entries
                    :desc "Create diary entry" "c" #'+diary/diary-create-entry
                    :desc "Quick add entry" "q" #'+diary/diary-quick-add
                    :desc "Recurring entry" "r" #'+diary/diary-create-recurring-entry
                    :desc "Go to date" "g" #'+diary/diary-goto-date
                    :desc "Insert template" "t" #'+diary/diary-insert-template
                    :desc "Initialize diary" "i" #'+diary/diary-initialize
                    :desc "Open diary file" "o" #'+diary/open-diary-file)))

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("D" . "diary")
                                 :desc "Show diary entries" "s" #'+diary/diary-show-entries
                                 :desc "Create diary entry" "c" #'+diary/diary-create-entry
                                 :desc "Quick add entry" "q" #'+diary/diary-quick-add
                                 :desc "Recurring entry" "r" #'+diary/diary-create-recurring-entry
                                 :desc "Go to date" "g" #'+diary/diary-goto-date
                                 :desc "Insert template" "t" #'+diary/diary-insert-template
                                 :desc "Initialize diary" "i" #'+diary/diary-initialize
                                 :desc "Open diary file" "o" #'+diary/open-diary-file))))

(map! :map calendar-mode-map
      :n "d" #'diary-view-entries
      :n "e" #'+diary/diary-create-entry
      :n "i" #'+diary/diary-create-entry
      :n "m" #'+diary/calendar-mark-diary-entries
      :n "s" #'+diary/diary-show-entries)

;; ----------------------------
;; EMMS Music Player Keybindings
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
                                 :desc "Volume up"          "+" #'app/emms-volume-raise
                                 :desc "Volume down"        "-" #'app/emms-volume-lower))))

(map! :map emms-playlist-mode-map
      "RET" #'emms-playlist-mode-play-smart
      "d" #'emms-playlist-mode-kill-track
      "u" #'emms-playlist-mode-undo
      "s" #'emms-shuffle)

;; ----------------------------
;; Everywhere Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("e" . "everywhere")
                                 :desc "Edit file with emacsclient (server-edit)" "f" #'server-edit
                                 :desc "Start server"  "s" #'server-start
                                 :desc "Stop server"   "k" #'server-force-delete
                                 :desc "Show server running status" "r" #'+everywhere/server-status))))


;; ----------------------------
;; IRC Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("i" . "irc")
                                 :desc "Connect to IRC"      "c" #'rcirc
                                 :desc "Switch IRC buffer"   "b" #'rcirc-switch-to-buffer
                                 :desc "Disconnect"          "q" #'rcirc-cmd-quit
                                 :desc "Reconnect"           "r" #'rcirc-reconnect-all
                                 :desc "List channels"       "l" #'rcirc-cmd-list))))

;; ----------------------------
;; RSS Keybindings
;; ----------------------------
(map! :map elfeed-search-mode-map
      :n "R" #'elfeed-mark-all-as-read
      :n "g r" #'elfeed-search-update--force)

(map! :map elfeed-show-mode-map
      :n "o" #'elfeed-show-visit-or-external
      :n "y" #'elfeed-show-yank)

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("a" . "applications")
                                (:prefix-map ("r" . "rss")
                                 :desc "Open RSS reader" "o" #'elfeed
                                 :desc "Update feeds"    "u" #'elfeed-update
                                 :desc "Search feeds"    "s" #'elfeed-search-set-filter
                                 :desc "Show entry"      "e" #'elfeed-show-entry
                                 :desc "Mark as read"    "m" #'elfeed-search-untag-all-unread
                                 :desc "Save entry to Org" "S" #'elfeed-save-entry-to-org
                                 :desc "Reset database" "R" #'elfeed-reset-database))))

;; ----------------------------
;; Grammar Keybindings
;; ----------------------------
(after! langtool
  (map! :leader
        (:prefix ("e" . "editor")
                 (:prefix ("c" . "checkers")
                          (:prefix ("g" . "grammar")
                           :desc "Grammar check buffer/region" "g" #'checkers-grammar-check-dwim
                           :desc "Grammar clear buffer" "G" #'langtool-correct-buffer
                           :desc "Show grammar message" "m" #'langtool-show-message-at-point
                           :desc "Grammar check done" "d" #'langtool-check-done)))))

;; ----------------------------
;; Spell Checkers Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("c" . "checkers")
                                (:prefix-map ("f" . "flyspell")
                                 :desc "Spell correct word (smart)" "c" #'checkers-spell-correct-dwim
                                 :desc "Spell check buffer" "b" #'flyspell-buffer
                                 :desc "Toggle prog mode spell" "p" #'checkers-spell-toggle-prog-mode
                                 :desc "Auto-correct word" "a" #'checkers-spell-auto-correct-word
                                 :desc "Switch language" "l" #'checkers-spell-switch-language
                                 :desc "Add word to dictionary" "w" #'ispell-word))))

;; ----------------------------
;; Syntax Checkers Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("c" . "checkers")
                                (:prefix-map ("s" . "syntax")
                                 :desc "List errors" "x" #'flycheck-list-errors
                                 :desc "Next error" "n" #'checkers-syntax-next-error-with-message
                                 :desc "Previous error" "p" #'checkers-syntax-previous-error-with-message
                                 :desc "Toggle flycheck" "t" #'checkers-syntax-toggle
                                 :desc "Clear errors" "c" #'checkers-syntax-clear-errors
                                 :desc "Verify setup" "v" #'flycheck-verify-setup
                                 :desc "Error at point" "." #'flycheck-display-error-at-point))))

;; ----------------------------
;; Default Config Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("Z" . "enhanced-editor")  ; Changed from "z" to "Z" to avoid conflicts
       ;; Buffer operations
       :desc "Cleanup buffer/region" "c" #'+default-config/cleanup-buffer-or-region
       :desc "Kill other buffers" "k" #'doom/kill-other-buffers
       :desc "Copy file path" "y" #'+default-config/copy-file-path
       :desc "Toggle window split" "t" #'+default-config/toggle-window-split

       ;; Text manipulation
       :desc "Duplicate line/region" "d" #'+default-config/duplicate-line-or-region
       :desc "Smart open line" "o" #'+default-config/smart-open-line
       :desc "Insert date" "D" #'+default-config/insert-date
       :desc "Insert timestamp" "T" #'+default-config/insert-timestamp

       ;; Line operations
       (:prefix-map ("l" . "line-ops")
        :desc "Move line up" "k" #'+default-config/move-line-up
        :desc "Move line down" "j" #'+default-config/move-line-down
        :desc "Sort lines" "s" #'sort-lines
        :desc "Reverse lines" "r" #'reverse-region
        :desc "Delete duplicate lines" "u" #'delete-duplicate-lines)

       ;; Text case operations
       (:prefix-map ("u" . "text-case")
        :desc "Upcase word" "w" #'upcase-word
        :desc "Downcase word" "l" #'downcase-word
        :desc "Capitalize word" "c" #'capitalize-word
        :desc "Upcase region" "U" #'upcase-region
        :desc "Downcase region" "L" #'downcase-region
        :desc "Capitalize region" "C" #'capitalize-region)

       ;; Advanced smartparens
       (:prefix-map ("s" . "smartparens-advanced")
        ;; Enhanced navigation
        :desc "Beginning of sexp" "a" #'sp-beginning-of-sexp
        :desc "End of sexp" "e" #'sp-end-of-sexp
        :desc "Down sexp" "d" #'sp-down-sexp
        :desc "Up sexp" "u" #'sp-up-sexp
        :desc "Backward down sexp" "D" #'sp-backward-down-sexp
        :desc "Backward up sexp" "U" #'sp-backward-up-sexp

        ;; Advanced manipulation
        :desc "Clone sexp" "c" #'sp-clone-sexp
        :desc "Transpose sexp" "t" #'sp-transpose-sexp
        :desc "Join sexp" "j" #'sp-join-sexp
        :desc "Split sexp" "S" #'sp-split-sexp
        :desc "Raise sexp" "r" #'sp-raise-sexp

        ;; Wrapping and unwrapping
        :desc "Wrap with ()" "(" #'sp-wrap-round
        :desc "Wrap with []" "[" #'sp-wrap-square
        :desc "Wrap with {}" "{" #'sp-wrap-curly
        :desc "Unwrap sexp" "w" #'sp-unwrap-sexp
        :desc "Backward unwrap" "W" #'sp-backward-unwrap-sexp

        ;; Slurping and barfing
        :desc "Forward slurp" ">" #'sp-forward-slurp-sexp
        :desc "Backward slurp" "<" #'sp-backward-slurp-sexp
        :desc "Forward barf" ")" #'sp-forward-barf-sexp
        :desc "Backward barf" "(" #'sp-backward-barf-sexp

        ;; Splicing
        :desc "Splice sexp" "s" #'sp-splice-sexp
        :desc "Splice killing forward" "f" #'sp-splice-sexp-killing-forward
        :desc "Splice killing backward" "b" #'sp-splice-sexp-killing-backward
        :desc "Splice killing around" "A" #'sp-splice-sexp-killing-around

        ;; Advanced operations
        :desc "Convolute sexp" "C" #'sp-convolute-sexp
        :desc "Absorb sexp" "B" #'sp-absorb-sexp
        :desc "Emit sexp" "E" #'sp-emit-sexp)

       ;; File operations
       (:prefix-map ("f" . "file-advanced")
        :desc "Delete trailing whitespace" "w" #'delete-trailing-whitespace
        :desc "Toggle truncate lines" "t" #'toggle-truncate-lines
        :desc "Toggle word wrap" "W" #'visual-line-mode
        :desc "Revert buffer" "r" #'revert-buffer
        :desc "Convert tabs to spaces" "T" #'untabify
        :desc "Convert spaces to tabs" "s" #'tabify)))

(map! :g
      "C-c d" #'+default-config/duplicate-line-or-region
      "C-c o" #'+default-config/smart-open-line
      "M-k" #'+default-config/smart-kill-whole-line
      "C-M-k" #'+default-config/move-line-up
      "C-M-j" #'+default-config/move-line-down
      "C-c C-t" #'+default-config/toggle-window-split
      "C-c y p" #'+default-config/copy-file-path)

(map! :i
      "C-<backspace>" #'+default-config/smart-backward-kill-word)

;; ----------------------------
;; Evil-Style Keybindings for hide/show
;; ----------------------------
(map! :map evil-normal-state-map
      "za" #'hs-toggle-hiding
      "zc" #'hs-hide-block
      "zo" #'hs-show-block
      "zM" #'hs-hide-all
      "zR" #'hs-show-all)

;; ----------------------------
;; Editor Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("f" . "format")
                    :desc "Toggle format on save" "t" #'+format/toggle
                    :desc "Format buffer" "f" #'+format/buffer
                    :desc "Format region" "r" #'+format/region
                    :desc "Show format log" "l" (lambda () (interactive) (switch-to-buffer +editor-format/format-log-buffer))
                    :desc "Clear format cache" "c" (lambda () (interactive) (clrhash +editor-format/format-cache)))

                   (:prefix-map ("t" . "tree-sitter")
                    :desc "Toggle format on save" "f" #'+format/toggle
                    :desc "Go to function start" "n" #'tree-sitter-goto-function-start
                    :desc "Go to function end" "b" #'tree-sitter-goto-function-end
                    :desc "Show tree" "t" #'tree-sitter-debug-mode)))

;; ----------------------------
;; Multiple Cursors Keybindings
;; ----------------------------
(map! :after evil
      ;; Normal mode bindings
      :n "g z n" #'evil-multiedit-match-symbol-and-next
      :n "g z p" #'evil-multiedit-match-symbol-and-prev
      :n "g z a" #'evil-multiedit-match-all
      :n "g z t" #'evil-multiedit-toggle-marker-here
      :n "g z r" #'evil-multiedit-restore

      ;; Visual mode bindings
      :v "R" #'evil-multiedit-match-all
      :v "C-n" #'evil-multiedit-match-symbol-and-next
      :v "C-p" #'evil-multiedit-match-symbol-and-prev
      :v "M-n" #'mc/mark-next-like-this
      :v "M-p" #'mc/mark-previous-like-this
      :v "M-a" #'mc/mark-all-like-this-dwim

      ;; Insert mode for quick access
      :i "C-M-n" #'mc/mark-next-like-this
      :i "C-M-p" #'mc/mark-previous-like-this)

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("m" . "multiedit")
                    :desc "Show help" "?" #'+multiedit/help)))

;; ----------------------------
;; Rainbow Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("k" . "Rainbow")
                    :desc "Toggle rainbow delimiter colors" "r c" #'rainbow-delimiters-toggle-colors
                    :desc "Increase delimiter contrast"    "r i" #'rainbow-delimiters-increase-contrast
                    :desc "Reset delimiter colors"         "r r" #'rainbow-delimiters-reset-colors
                    :desc "Toggle rainbow delimiters"      "r d" #'rainbow-delimiters-mode)))

;; ----------------------------
;; Dirvish Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("d" . "dirvish")
       :desc "Open Dirvish" "d" #'dirvish
       :desc "Dirvish current directory" "." #'dirvish-dwim
       :desc "Dirvish other window" "o" #'dirvish-other-window
       :desc "Quick access" "a" #'dirvish-quick-access
       :desc "Dirvish fd search" "f" #'dirvish-fd
       :desc "Dirvish history" "h" #'dirvish-history-jump))

;; ----------------------------
;; Electric Keybindings
;; ----------------------------
(map! :map prog-mode-map
      :i "RET" #'+electric/smart-newline-and-indent
      :i ";"   #'+electric/smart-semicolon
      :i "}"   #'+electric/smart-closing-brace)

;; ----------------------------
;; EWW Keybindings
;; ----------------------------
(map! :map eww-mode-map
      :n "o" #'eww
      :n "O" #'+eww/eww-browse-url-new-buffer
      :n "f" #'eww-lnum-follow
      :n "F" #'eww-lnum-universal
      :n "r" #'eww-reload
      :n "R" #'+eww/eww-readable-mode
      :n "H" #'eww-back-url
      :n "L" #'eww-forward-url
      :n "n" #'eww-next-url
      :n "p" #'eww-previous-url
      :n "u" #'eww-up-url
      :n "t" #'eww-top-url
      :n "d" #'eww-download
      :n "w" #'eww-copy-page-url
      :n "b" #'eww-add-bookmark
      :n "B" #'eww-list-bookmarks
      :n "s" #'+eww/eww-search-engine
      :n "S" #'eww-search-words
      :n "i" #'+eww/eww-toggle-images
      :n "x" #'+eww/eww-browse-with-external-browser
      :n "q" #'quit-window
      :n "Q" #'eww-quit
      :n "g" #'eww-reload
      :n "G" #'end-of-buffer
      :n "gg" #'beginning-of-buffer
      :n "/" #'eww-search-words
      :n "?" #'eww-search-words
      :n "TAB" #'shr-next-link
      :n "S-TAB" #'shr-previous-link)

(map! :leader
      (:prefix-map ("o" . "open")
       :desc "EWW web browser" "w" #'eww
       :desc "EWW new buffer" "W" #'my/eww-browse-url-new-buffer
       :desc "EWW bookmarks" "b" #'eww-list-bookmarks))

;; ----------------------------
;; IBuffers Keybindings
;; ----------------------------
(map! :map ibuffer-mode-map
      :n "TAB" #'ibuffer-toggle-filter-group
      :n "RET" #'ibuffer-visit-buffer
      :n "o"   #'ibuffer-visit-buffer-other-window
      :n "O"   #'+ibuffer/ibuffer-visit-buffer-other-window-and-close
      :n "d"   #'ibuffer-mark-for-delete
      :n "u"   #'ibuffer-unmark-forward
      :n "x"   #'ibuffer-do-kill-on-deletion-marks
      :n "s"   #'ibuffer-do-sort-by-alphabetic
      :n "S"   #'ibuffer-do-sort-by-major-mode
      :n "r"   #'ibuffer-update
      :n "g"   #'ibuffer-update
      :n "q"   #'quit-window
      :n "h"   #'describe-mode
      :n "?"   #'describe-mode
      :n "t"   #'+ibuffer/ibuffer-toggle-grouping
      :n "K"   #'+ibuffer/ibuffer-kill-all-buffers-in-group
      :n "N"   #'+ibuffer/ibuffer-create-new-group
      ;; Filter shortcuts
      :n "f n" #'ibuffer-filter-by-name
      :n "f m" #'ibuffer-filter-by-mode
      :n "f f" #'ibuffer-filter-by-filename
      :n "f c" #'ibuffer-filter-by-content
      :n "f s" #'ibuffer-filter-by-size-gt
      :n "f S" #'ibuffer-filter-by-size-lt
      :n "f /" #'ibuffer-filter-disable
      ;; Marking shortcuts
      :n "m m" #'ibuffer-mark-forward
      :n "m d" #'ibuffer-mark-for-delete
      :n "m s" #'ibuffer-mark-special-buffers
      :n "m o" #'ibuffer-mark-old-buffers
      :n "m r" #'ibuffer-mark-read-only-buffers
      :n "m u" #'ibuffer-mark-unsaved-buffers
      :n "m M" #'ibuffer-mark-by-mode
      :n "m n" #'ibuffer-mark-by-name-regexp
      :n "m f" #'ibuffer-mark-by-file-name-regexp
      :n "U"   #'ibuffer-unmark-all)

(map! :leader
      :desc "Open Ibuffer" "b i" #'ibuffer
      :desc "Ibuffer (other window)" "b I" #'ibuffer-other-window)

;; ----------------------------
;; Ranger Keybindings
;; ----------------------------
(map! :leader
      :desc "Open Ranger" "o r" #'ranger)

;; ----------------------------
;; Undo Keybindings
;; ----------------------------
(with-eval-after-load 'undo-tree
  (let ((map undo-tree-visualizer-mode-map))
    (define-key map (kbd "RET") #'undo-tree-visualizer-quit)
    (define-key map (kbd "q")   #'undo-tree-visualizer-quit)
    (define-key map (kbd "j")   #'undo-tree-visualize-undo)
    (define-key map (kbd "k")   #'undo-tree-visualize-redo)
    (define-key map (kbd "h")   #'undo-tree-visualize-switch-branch-left)
    (define-key map (kbd "l")   #'undo-tree-visualize-switch-branch-right)))

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("e" . "emacs")
                                (:prefix-map ("u" . "undo-tree")
                                 :desc "Undo"       "u" #'undo-tree-undo
                                 :desc "Redo"       "r" #'undo-tree-redo
                                 :desc "Visualizer" "v" #'undo-tree-visualize))))

;; ----------------------------
;; Yasnippet Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("y" . "yasnippet")
                    :desc "Insert snippet"        "i" #'ivy-yasnippet
                    :desc "New snippet"           "n" #'+yas/yas-new-snippet
                    :desc "Visit snippet file"    "v" #'yas-visit-snippet-file
                    :desc "Find snippet"          "f" #'yas-find-snippets
                    :desc "Reload all snippets"   "r" #'yas-reload-all
                    :desc "Create snippet from region" "c" #'yas-new-snippet-with-content)))

;; ----------------------------
;; Eshell Keybindings
;; ----------------------------
(map! :leader
      :desc "Eshell" "o e" #'eshell)

;; ----------------------------
;; Vterm Keybindings
;; ----------------------------
(map! :leader
      :desc "Vterm" "o t" #'vterm)

;; ----------------------------
;; Collab Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("c" . "collaboration")
                                 ;; Core session management
                                 :desc "Share buffer"             "s" #'+crt/crdt-share-buffer-enhanced
                                 :desc "Connect to session"      "c" #'+crt/crdt-connect-enhanced
                                 :desc "Disconnect/Stop"          "d" #'+crt/crdt-stop-all-sessions
                                 :desc "Reconnect last"           "r" #'+crt/crdt-reconnect-last
                                 :desc "Kill all sessions"        "k" #'+crt/crdt-stop-all-sessions

                                 ;; Information and monitoring
                                 :desc "List sessions"            "l" #'+crt/crdt-list-active-sessions
                                 :desc "Show users"               "u" #'+crt/crdt-show-connected-users
                                 :desc "Session status"           "i" #'+crt/crdt-status
                                 :desc "Health check"             "h" #'+crt/crdt-health-check

                                 ;; UI and preferences
                                 :desc "Toggle user cursors"      "t" #'+crt/crdt-toggle-user-cursors
                                 :desc "Send message"             "m" #'+crt/crdt-send-message

                                 ;; Utilities
                                 (:prefix ("o" . "options")
                                  :desc "Set default port"        "p" (lambda () (interactive)
                                                                        (setq +crt/crdt-default-port
                                                                              (read-number "Default port: " +crt/crdt-default-port))
                                                                        (message "Default port set to %d" +crt/crdt-default-port))
                                  :desc "Set default address"     "a" (lambda () (interactive)
                                                                        (setq +crt/crdt-default-address
                                                                              (read-string "Default address: " +crt/crdt-default-address))
                                                                        (message "Default address set to %s" +crt/crdt-default-address))
                                  :desc "Toggle auto-save"        "s" (lambda () (interactive)
                                                                        (setq +crt/crdt-auto-save-enabled
                                                                              (not +crt/crdt-auto-save-enabled))
                                                                        (message "Auto-save for shared buffers: %s"
                                                                                 (if +crt/crdt-auto-save-enabled "✓ enabled" "✗ disabled"))))))))

;; ----------------------------
;; DAP Debugger Keybinding
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix-map ("d" . "debug")
                                 ;; Core debugging operations
                                 :desc "Debug smart/start"        "d" #'+dap/dap-debug-smart
                                 :desc "Debug with template"      "t" #'+dap/dap-debug-with-template
                                 :desc "Debug last session"       "l" #'dap-debug-last
                                 :desc "Restart session"          "r" #'dap-debug-restart
                                 :desc "Quit/disconnect"          "q" #'dap-disconnect
                                 :desc "Kill all sessions"        "k" #'+dap/dap-kill-all-sessions
                                 :desc "Continue execution"       "c" #'+dap/dap-continue-or-start

                                 ;; Breakpoint management
                                 (:prefix ("b" . "breakpoints")
                                  :desc "Toggle breakpoint"       "b" #'dap-breakpoint-toggle
                                  :desc "Conditional breakpoint"  "c" #'+dap/dap-toggle-breakpoint-with-condition
                                  :desc "Delete all breakpoints"  "D" #'+dap/dap-clear-all-breakpoints
                                  :desc "List breakpoints"        "l" #'dap-ui-breakpoints)

                                 ;; Stepping operations
                                 (:prefix ("s" . "step")
                                  :desc "Step over"               "o" #'dap-next
                                  :desc "Step into"               "i" #'dap-step-in
                                  :desc "Step out"                "u" #'dap-step-out
                                  :desc "Continue"                "c" #'dap-continue)

                                 ;; Expression evaluation
                                 (:prefix ("e" . "eval")
                                  :desc "Eval at point"           "e" #'dap-eval-thing-at-point
                                  :desc "Eval region/symbol"      "r" #'+dap/dap-eval-region-or-symbol
                                  :desc "Eval expression"         "E" #'dap-eval)

                                 ;; Watch expressions
                                 (:prefix ("w" . "watch")
                                  :desc "Add watch"               "a" #'+dap/dap-add-watch-expression
                                  :desc "Show expressions"        "w" #'dap-ui-expressions)

                                 ;; UI and windows
                                 (:prefix ("u" . "ui")
                                  :desc "Show many windows"       "m" #'+dap/dap-ui-many-windows
                                  :desc "Hide windows"            "h" #'+dap/dap-ui-hide-all
                                  :desc "Show locals"             "l" #'dap-ui-locals
                                  :desc "Show breakpoints"        "b" #'dap-ui-breakpoints
                                  :desc "Show expressions"        "e" #'dap-ui-expressions
                                  :desc "Show sessions"           "s" #'dap-ui-sessions
                                  :desc "Restore layout"          "R" #'+dap/dap-restore-window-layout)))))

(map! "C-<f5>"   #'+dap/dap-debug-smart
      "<f5>"     #'dap-continue
      "S-<f5>"   #'dap-disconnect
      "<f9>"     #'dap-breakpoint-toggle
      "<f10>"    #'dap-next
      "<f11>"    #'dap-step-in
      "S-<f11>"  #'dap-step-out)


;; ----------------------------
;; DirEnv Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                ;; Minimal direnv prefix - only essential commands
                                (:prefix ("e" . "environment")
                                 :desc "Edit .envrc"              "e" #'+direnv/direnv-find-envrc
                                 :desc "Reload (manual)"          "r" #'+direnv/direnv-manual-reload
                                 :desc "Allow .envrc"             "a" #'+direnv/direnv-allow
                                 :desc "Deny .envrc"              "d" #'+direnv/direnv-deny
                                 :desc "Toggle performance mode"  "p" #'+direnv/direnv-toggle-performance-mode
                                 :desc "Status"                   "s" #'+direnv/direnv-status))))

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("e" . "environment")
                                 :desc "Refresh LSP after env change" "l" #'+direnv/direnv-lsp-refresh))))


;; ----------------------------
;; Docker Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("o" . "devops")
                                         (:prefix ("d" . "docker")
                                                  ;; Container management
                                                  (:prefix ("c" . "containers")
                                                   :desc "List containers"         "c" #'+docker/docker-containers
                                                   :desc "Quick exec"              "e" #'+docker/docker-quick-exec
                                                   :desc "Quick logs"              "l" #'+docker/docker-quick-logs
                                                   :desc "Container stats"         "s" #'+docker/docker-stats
                                                   :desc "Container top"           "t" #'+docker/docker-top)
                                                  ;; Image management
                                                  (:prefix ("i" . "images")
                                                   :desc "List images"             "i" #'+docker/docker-images
                                                   :desc "Build image"             "b" #'+docker/docker-build
                                                   :desc "Run image"               "r" #'+docker/docker-run
                                                   :desc "Pull image"              "p" #'docker-image-pull)
                                                  ;; Network management
                                                  (:prefix ("n" . "networks")
                                                   :desc "List networks"           "n" #'docker-networks
                                                   :desc "Create network"          "c" #'docker-network-create)
                                                  ;; Volume management
                                                  (:prefix ("v" . "volumes")
                                                   :desc "List volumes"            "v" #'docker-volumes
                                                   :desc "Create volume"           "c" #'docker-volume-create)
                                                  ;; Docker Compose operations
                                                  (:prefix ("C" . "compose")
                                                   :desc "Up"                      "u" #'+docker/docker-compose-up
                                                   :desc "Down"                    "d" #'+docker/docker-compose-down
                                                   :desc "Logs"                    "l" #'+docker/docker-compose-logs
                                                   :desc "Build"                   "b" #'+docker/docker-compose-build
                                                   :desc "Pull"                    "p" #'+docker/docker-compose-pull
                                                   :desc "Restart"                 "r" #'+docker/docker-compose-restart)
                                                  ;; Build operations
                                                  (:prefix ("b" . "build")
                                                   :desc "Build image"             "b" #'+docker/docker-build
                                                   :desc "Compose build"           "c" #'+docker/docker-compose-build)
                                                  ;; Logs and monitoring
                                                  (:prefix ("l" . "logs")
                                                   :desc "Container logs"          "c" #'+docker/docker-quick-logs
                                                   :desc "Compose logs"            "C" #'+docker/docker-compose-logs
                                                   :desc "Container stats"         "s" #'+docker/docker-stats)
                                                  ;; System operations
                                                  (:prefix ("s" . "system")
                                                   :desc "System info"             "i" #'+docker/docker-show-system-info
                                                   :desc "System prune"            "p" #'+docker/docker-system-prune
                                                   :desc "System stats"            "s" #'+docker/docker-stats)
                                                  ;; Templates and scaffolding
                                                  (:prefix ("t" . "templates")
                                                   :desc "Create Dockerfile"       "d" #'+docker/docker-create-dockerfile
                                                   :desc "Create Compose file"     "c" #'+docker/docker-create-compose-file)
                                                  ;; Help and documentation
                                                  (:prefix ("h" . "help")
                                                   :desc "Docker help"             "d"
                                                   (lambda () (interactive) (async-shell-command "docker --help" "*Docker Help*"))
                                                   :desc "Compose help"            "c"
                                                   (lambda () (interactive) (async-shell-command (concat (+docker/compose-cmd) " --help") "*Docker Compose Help*"))
                                                   :desc "System info"             "i" #'+docker/docker-show-system-info))))))

;; ----------------------------
;; Editor Config Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("e" . "editor-config")
                                 :desc "Edit .editorconfig"    "e" #'+editorconfig/editorconfig-find-file
                                 :desc "Create from template"  "t" #'+editorconfig/editorconfig-create-file
                                 :desc "Validate syntax"       "v" #'+editorconfig/editorconfig-validate-file
                                 :desc "Show settings"         "c" #'+editorconfig/editorconfig-show-current-settings
                                 :desc "Reload buffer"         "r" #'+editorconfig/editorconfig-reload-buffer
                                 :desc "Apply to project"      "a" #'+editorconfig/editorconfig-apply-to-project
                                 :desc "Debug info"            "d" #'+editorconfig/editorconfig-show-debug-info
                                 :desc "Toggle debug mode"     "D" #'+editorconfig/editorconfig-toggle-debug-mode
                                 :desc "Toggle auto-apply"     "A" #'+editorconfig/editorconfig-toggle-auto-apply
                                 :desc "Help"                  "h" (lambda () (interactive) (browse-url "https://editorconfig.org/"))))))

;; ----------------------------
;; Eval Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("v" . "eval")
                                         (:when (modulep! :lang emacs-lisp)
                                           :desc "Eval expression" "e" #'eval-last-sexp
                                           :desc "Eval buffer" "b" #'eval-buffer
                                           :desc "Eval region" "r" #'eval-region
                                           :desc "Eval defun" "d" #'eval-defun
                                           :desc "Eval and replace" "R" #'eval-and-replace
                                           :desc "Eval expression (pretty)" "p" #'pp-eval-last-sexp
                                           :desc "Eval expression in IELM" "i" #'ielm-eval-last-sexp)

                                         ;; General evaluation commands
                                         :desc "Eval expression at point" "." #'+eval/region-and-replace
                                         :desc "Send to REPL" "s" #'+eval/send-region-to-repl
                                         :desc "Open REPL" "o" #'+eval/open-repl-other-window))))


;; ----------------------------
;; AI LLM Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("a" . "ai-tools")
                                         ;; Code assistance
                                         (:prefix ("c" . "code")
                                          :desc "Explain code" "e" #'+ai/explain-code
                                          :desc "Function docs" "d" #'+ai/create-function-docs
                                          :desc "Smart commit" "c" #'+ai/smart-commit-message)
                                         
                                         ;; Learning tools
                                         (:prefix ("l" . "learning")
                                          :desc "Learning notes" "n" #'+ai/create-learning-notes
                                          :desc "Practice exercises" "e" #'+ai/generate-practice-exercises)
                                         
                                         ;; Project tools
                                         (:prefix ("p" . "project")
                                          :desc "Analyze project" "a" #'+ai/analyze-project-structure)))))

;; ----------------------------
;; Lookup Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix-map ("l" . "lookup") 
                                             (:prefix ("s" . "search")
                                              ;; Core lookup functions (using 's' prefix since 'l' might conflict)
                                              :desc "Definition at point" "d" #'+lookup/smart-lookup-at-point
                                              :desc "References" "r" #'+lookup/references
                                              :desc "Documentation" "h" #'+lookup/documentation
                                              :desc "Dictionary" "D" #'define-word-at-point)
                                             
                                             ;; user-specific searches
                                             (:prefix ("d" . "documentation")
                                              :desc "Search my repos" "r" #'+lookup/search-in-user-repos
                                              :desc "Current repo docs" "d" #'+lookup/browse-current-repo-docs)
                                             
                                             ;; Language-specific lookups
                                             (:prefix ("l" . "language")
                                              :desc "JavaScript MDN" "j" #'+lookup/javascript-mdn
                                              :desc "NPM package" "n" #'+lookup/npm-package
                                              :desc "Rust docs" "r" #'+lookup/rust-docs
                                              :desc "GitHub search" "g" #'+lookup/github-search)
                                             
                                             ;; Learning tools
                                             (:prefix ("L" . "learning")
                                              :desc "Explain concept" "e" #'+lookup/explain-concept
                                              :desc "Code examples" "c" #'+lookup/code-examples
                                              :desc "Learning resources" "r" #'+lookup/learning-resources
                                              :desc "Define term" "d" #'+lookup/define-programming-term)
                                             
                                             ;; Documentation browsers
                                             (:prefix ("o" . "online")
                                              :desc "Arch Wiki" "a" #'+lookup/search-arch-wiki
                                              :desc "DevDocs" "d" #'devdocs-lookup
                                              :desc "Dash docs" "D" #'dash-docs-search)))))

;; ----------------------------
;; LSP Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix ("s" . "lsp")
                                 ;; Workspace management
                                 :desc "Start LSP"                "s" #'lsp
                                 :desc "Start deferred"           "S" #'lsp-deferred
                                 :desc "Restart workspace"        "r" #'+lsp/lsp-workspace-restart
                                 :desc "Shutdown workspace"       "q" #'+lsp/lsp-workspace-cleanup
                                 :desc "Clear cache"              "c" #'+lsp/lsp-clear-cache
                                 :desc "Workspace info"           "i" #'+lsp/lsp-show-workspace-info
                                 :desc "Performance report"       "p" #'+lsp/lsp-performance-report
                                 
                                 ;; Treemacs integration
                                 :desc "Treemacs symbols"         "t" #'lsp-treemacs-symbols
                                 :desc "Treemacs errors"          "e" #'lsp-treemacs-errors-list
                                 
                                 ;; UI toggles
                                 :desc "Toggle sideline"          "u" #'lsp-ui-sideline-mode
                                 :desc "Toggle doc mode"          "d" #'lsp-ui-doc-mode
                                 :desc "Toggle diagnostics"       "D" #'+lsp/lsp-toggle-diagnostics
                                 :desc "Toggle highlighting"      "h" #'+lsp/lsp-toggle-symbol-highlighting
                                 :desc "Toggle breadcrumb"        "b" #'+lsp/lsp-toggle-breadcrumb
                                 :desc "Toggle lens"              "l" #'+lsp/lsp-toggle-lens))))

(map! :leader
      (:prefix-map ("c" . "code")
       :desc "Start LSP"                "L" #'lsp
       :desc "LSP workspace info"       "I" #'+lsp/lsp-show-workspace-info
       :desc "LSP restart"              "R" #'+lsp/lsp-workspace-restart))

;; ----------------------------
;; Make Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix-map ("m" . "make")
                                 :desc "Smart command" "r" #'+make/smart-command
                                 :desc "Run make" "m" #'+make/run
                                 :desc "Run last" "l" #'+make/run-last
                                 :desc "View buffer" "v" #'+make/view-buffer
                                 :desc "Project info" "i" #'+make/show-project-info
                                 :desc "Create Makefile" "c" #'+make/create-makefile))))

(when (and (featurep 'projectile) (projectile-project-name))
  (let ((project (projectile-project-name)))
    (cond
     ((string-match-p "arch-frame" project)
      (map! :leader
            (:prefix "c m"
             :desc "Install config" "I" #'+make/arch-install)))
     
     ((string-match-p "learning-javascript" project)
      (map! :leader
            (:prefix "c m"
             :desc "Dev server" "d" #'+make/js-dev)))
     
     ((string-match-p "bdostumski\\.github\\.io" project)
      (map! :leader
            (:prefix "c m"
             :desc "Serve site" "s" #'+make/serve-site))))))

;; ----------------------------
;; Password Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix ("t" . "tools")
                            (:prefix ("p" . "password")
                             :desc "Status" "s" #'+pass/status
                             :desc "Initialize" "I" #'+pass/initialize
                             :desc "Setup GPG" "G" #'+pass/setup-gpg
                             :desc "Reload config" "R" #'+pass/reload-config
                             :desc "Copy" "c" #'+pass/copy-password
                             :desc "Generate" "g" #'+pass/generate-password
                             :desc "Insert" "i" #'+pass/insert-password
                             :desc "List" "l" #'+pass/list-passwords))))

;; ----------------------------
;; Projectile Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("t" . "tools")
                                (:prefix-map ("p" . "project")
                                 ;; Core projectile commands
                                 :desc "Switch project" "p" #'projectile-switch-project
                                 :desc "Find file in project" "f" #'projectile-find-file
                                 :desc "Recent files" "r" #'projectile-recentf
                                 :desc "Search in project" "s" #'+projectile/search-project
                                 :desc "Replace in project" "R" #'projectile-replace
                                 :desc "Toggle implementation/test" "t" #'projectile-toggle-between-implementation-and-test
                                 :desc "Run project" "c" #'projectile-compile-project
                                 :desc "Test project" "T" #'projectile-test-project
                                 
                                 ;; Enhanced project management
                                 :desc "Project info" "i" #'+projectile/project-info
                                 :desc "Recent projects" "P" #'+projectile/recent-projects
                                 :desc "Create new project" "n" #'+projectile/create-project
                                 
                                 ;; Project discovery and cleanup
                                 :desc "Discover projects" "D" #'+projectile/discover-projects
                                 :desc "Cleanup stale projects" "C" #'+projectile/cleanup-stale-projects
                                 
                                 ;; Favorites
                                 (:prefix-map ("F" . "favorites")
                                  :desc "Add to favorites" "a" #'+projectile/add-to-favorites
                                  :desc "Remove from favorites" "r" #'+projectile/remove-from-favorites
                                  :desc "Switch to favorite" "f" #'+projectile/switch-to-favorite)
                                 
                                 ;; Git integration
                                 :desc "Magit status" "g" #'projectile-vc
                                 :desc "Git grep" "G" #'projectile-grep
                                 
                                 ;; Utilities
                                 :desc "Find directory" "d" #'projectile-find-dir
                                 :desc "Find other file" "o" #'projectile-find-other-file
                                 :desc "Kill project buffers" "k" #'projectile-kill-buffers
                                 :desc "Save project buffers" "S" #'projectile-save-project-buffers
                                 :desc "Invalidate cache" "I" #'projectile-invalidate-cache))))

;; ----------------------------
;; RGB Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("r" . "colors")
                         ;; Basic color operations
                         :desc "Increase color" "+" #'rgb-increase-color
                         :desc "Decrease color" "-" #'rgb-decrease-color
                         :desc "Color at point" "." #'+rgb/color-at-point
                         :desc "Adjust color" "a" #'+rgb/adjust-color-at-point
                         
                         ;; Color generation
                         :desc "Theme colors" "t" #'+rgb/current-theme-colors
                         :desc "Random color" "r" #'+rgb/generate-random-color
                         :desc "Insert CSS color" "i" #'+rgb/insert-css-color
                         :desc "Create gradient" "g" #'+rgb/create-gradient
                         
                         ;; Palettes and collections
                         (:prefix ("p" . "palettes")
                          :desc "Show palette" "s" #'+rgb/show-palette
                          :desc "Add custom palette" "a" #'+rgb/add-custom-palette
                          :desc "Extract from buffer" "e" #'+rgb/extract-colors-from-buffer)
                         
                         ;; Accessibility
                         (:prefix ("A" . "accessibility")
                          :desc "Check contrast" "c" #'+rgb/check-contrast)))))

(map! :map (css-mode-map scss-mode-map web-mode-map)
      :localleader
      (:prefix ("r" . "colors")
       :desc "Color info" "i" #'+rgb/color-at-point
       :desc "Insert color" "n" #'+rgb/insert-css-color
       :desc "Adjust color" "a" #'+rgb/adjust-color-at-point
       :desc "Create gradient" "g" #'+rgb/create-gradient
       :desc "Check contrast" "c" #'+rgb/check-contrast
       :desc "Extract colors" "e" #'+rgb/extract-colors-from-buffer))

;; ----------------------------
;; Taskrunner Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("r" . "task-runner")
                         ;; Core task operations
                         :desc "Run task" "r" #'+taskrunner/run-task
                         :desc "Run last task" "l" #'+taskrunner/run-last-task
                         :desc "Discover tasks" "d" #'+taskrunner/discover-tasks
                         :desc "Edit task file" "e" #'+taskrunner/edit-task-file
                         
                         ;; Task management
                         :desc "Create task file" "c" #'+taskrunner/create-taskfile
                         :desc "Add to favorites" "f" #'+taskrunner/add-to-favorites
                         :desc "Run favorite" "F" #'+taskrunner/run-favorite
                         
                         ;; Quick common tasks
                         :desc "Build" "b" (lambda () (interactive) (+taskrunner/quick-task "build"))
                         :desc "Test" "T" (lambda () (interactive) (+taskrunner/quick-task "test"))
                         :desc "Clean" "C" (lambda () (interactive) (+taskrunner/quick-task "clean"))))))

;; ----------------------------
;; Testing Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("g" . "testing")
                         ;; Core test operations
                         :desc "Run test at point" "t" #'+testing/run-test-at-point
                         :desc "Run test file" "f" #'+testing/run-test-file
                         :desc "Run project tests" "p" #'+testing/run-project-tests
                         :desc "Run last test" "l" #'+testing/run-last-test
                         
                         ;; Test management
                         :desc "Discover tests" "d" #'+testing/discover-tests
                         :desc "Create test file" "n" #'+testing/create-test-file
                         :desc "Toggle test/source" "T" #'+testing/toggle-test-file
                         
                         ;; Coverage and reporting
                         :desc "Run with coverage" "c" #'+testing/run-with-coverage
                         :desc "View coverage report" "C" #'+testing/view-coverage-report
                         
                         ;; Favorites and history
                         :desc "Add to favorites" "F" #'+testing/add-to-favorites
                         :desc "Run favorite" "r" #'+testing/run-favorite))))

(map! :map (js-mode-map typescript-mode-map)
      :localleader
      (:prefix ("T" . "Test")
       :desc "Run Jest test" "t" #'+testing/run-jest-at-point
       :desc "Run Jest file" "f" #'+testing/run-jest-file
       :desc "Jest coverage" "c" (lambda () (interactive) (compile "npx jest --coverage"))))

(map! :map python-mode-map
      :localleader
      (:prefix ("T" . "Test")
       :desc "Run pytest" "t" #'+testing/run-pytest-at-point
       :desc "Run pytest file" "f" #'+testing/run-pytest-file
       :desc "Pytest coverage" "c" (lambda () (interactive) (compile "pytest --cov=."))))

(map! :map rust-mode-map
      :localleader
      (:prefix ("T" . "Test")
       :desc "Run Cargo test" "t" #'+testing/run-cargo-test-at-point
       :desc "Run Cargo file tests" "f" #'+testing/run-cargo-test-file
       :desc "Cargo coverage" "c" (lambda () (interactive) (compile "cargo tarpaulin"))))

;; ----------------------------
;; TMUX Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("x" . "tmux")  ;; Using capital T to avoid conflicts
                         ;; Core session management
                         :desc "New session" "n" #'+tmux/new-session
                         :desc "Switch session" "s" #'+tmux/switch-session
                         :desc "Kill session" "k" #'+tmux/kill-session
                         :desc "Session info" "i" #'+tmux/session-info
                         
                         ;; Project integration
                         :desc "Create project session" "p" #'+tmux/create-project-session
                         :desc "Development workflow" "w" #'+tmux/development-workflow
                         :desc "Quick command" "q" #'+tmux/quick-command
                         
                         ;; Advanced operations
                         :desc "Send command" "c" #'+tmux/send-command
                         :desc "Send region" "r" #'+tmux/send-region-to-pane
                         :desc "Attach session" "a" #'+tmux/attach-to-session
                         :desc "List panes" "l" #'+tmux/list-all-panes
                         :desc "Save layout" "S" #'+tmux/save-session-layout
                         :desc "Restore layout" "R" #'+tmux/restore-session-layout))))

;; ----------------------------
;; Treesitter Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("s" . "tree-sitter")
                         ;; Navigation
                         :desc "Function start" "f" #'+tree-sitter/goto-function-start
                         :desc "Function end" "e" #'+tree-sitter/goto-function-end
                         :desc "Function info" "i" #'+tree-sitter/function-info
                         
                         ;; NEW: Folding
                         :desc "Fold function" "c" #'+tree-sitter/fold-defun
                         :desc "Fold all functions" "a" #'+tree-sitter/fold-all-defuns
                         
                         ;; NEW: Visualization
                         :desc "Visualize node" "v" #'+tree-sitter/visualize-current-node
                         :desc "Performance report" "p" #'+tree-sitter/performance-report
                         
                         ;; Management
                         :desc "Recover from errors" "r" #'+tree-sitter/recover-from-errors
                         :desc "Grammar status" "s" #'+tree-sitter/check-grammar-status
                         :desc "Install grammars" "g" #'+tree-sitter/ensure-grammars))))

;; ----------------------------
;; Upload Keybindings
;; ----------------------------
(map! :leader
      (:prefix ("e" . "editor")
               (:prefix ("t" . "tools")
                        (:prefix ("u" . "upload")
                         :desc "Upload project" "p" #'+upload/upload-project
                         :desc "Deploy to GitHub Pages" "g" #'+upload/deploy-to-github-pages
                         :desc "Deployment status" "s" #'+upload/show-deployment-status))))

;; ----------------------------
;; Deft Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("u" . "ui")
                                (:prefix ("d" . "deft")
                                 :desc "deft-notes" "d" #'deft))))

;; ----------------------------
;; Minimap Keybindings
;; ----------------------------
(map! :leader
      :desc "Toggle minimap" "t m" #'minimap-mode)

;; ----------------------------
;; Treemacs Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("u" . "ui")
                                (:prefix ("t" . "treemacs")
                                 :desc "Toggle Treemacs" "t" #'treemacs
                                 :desc "Treemacs Find File" "f" #'treemacs-find-file
                                 :desc "Treemacs Follow Mode" "F" #'treemacs-follow-mode))))

;; ----------------------------
;; Workspace Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("u" . "ui")
                                (:prefix ("w" . "workspace")
                                 :desc "Next workspace"      "n"  #'+workspace/switch-right
                                 :desc "Previous workspace"  "b"  #'+workspace/switch-left
                                 :desc "Switch workspace"    "w"  #'+workspace/switch-to
                                 :desc "New workspace"       "W"  #'+workspace/new
                                 :desc "Delete workspace"    "D"  #'+workspace/delete))))

;; ----------------------------------------
;; Zen Keybindings
;; ----------------------------------------
(map! :leader
      :desc "Toggle Zen Mode" "t z" #'writeroom-mode)

;; ----------------------------------------
;; Scala Keybindings
;; ----------------------------------------
(map! :after scala-mode
      :map scala-mode-map
      :localleader
      :desc "Start sbt" "s" #'sbt-start
      :desc "Run current test" "t c" #'sbt-do-test-only
      :desc "Run all tests" "t a" #'sbt-do-test
      :desc "Run REPL" "r" #'sbt-run-repl
      :desc "Compile" "c" #'sbt-do-compile
      :desc "Go to definition" "d" #'lsp-find-definition
      :desc "Format buffer" "f" #'lsp-format-buffer)

;; ----------------------------
;; Web Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("w" . "web")
                                 :desc "Format buffer"      "f" #'lsp-format-buffer
                                 :desc "Go to definition"   "d" #'lsp-find-definition
                                 :desc "Find references"    "r" #'lsp-find-references
                                 :desc "Rename symbol"      "n" #'lsp-rename
                                 :desc "Show errors"        "e" #'flycheck-list-errors
                                 :desc "Run npm script"     "s" #'npm-run-current-project
                                 :desc "Run dev server"     "d" (cmd! (npm-run-current-project "dev"))
                                 :desc "Build project"      "b" (cmd! (npm-run-current-project "build"))
                                 :desc "Test project"       "t" (cmd! (npm-run-current-project "test"))))))

;; ----------------------------
;; SQL Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("s" . "sql")
                                 :desc "Connect to DB"           "c" #'sql-connect
                                 :desc "Execute buffer"          "e" #'sql-send-buffer
                                 :desc "Execute region"          "r" #'sql-send-region
                                 :desc "List products"           "l" #'sql-list-all-products
                                 :desc "Set product"             "s" #'sql-set-product))))

;; ----------------------------
;; Go Keybindings
;; ----------------------------
(map! :map go-mode-map
      :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix ("g" . "go")
                                 :desc "Run go build"          "b" #'go-build
                                 :desc "Run go test"           "t" #'go-test-current-project
                                 :desc "Run current test"      "T" #'go-test-current-test
                                 :desc "Run go test with coverage" "c" #'go-test-current-coverage
                                 :desc "Go to definition"      "d" #'lsp-find-definition
                                 :desc "Go to references"      "r" #'lsp-find-references
                                 :desc "Add import"            "i" #'go-import-add
                                 :desc "Remove unused imports" "I" #'go-remove-unused-imports
                                 :desc "Toggle test file"      "f" #'go-test-toggle
                                 :desc "Run go mod tidy"       "m" (cmd! (compile "go mod tidy"))))))

;; ----------------------------
;; YAML Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("y" . "yaml")
                                 :desc "Format buffer"      "f" #'lsp-format-buffer
                                 :desc "Go to definition"   "d" #'lsp-find-definition
                                 :desc "Show diagnostics"   "v" #'lsp-ui-flycheck-list
                                 :desc "Organize imports"   "o" #'lsp-organize-imports
                                 :desc "Rename"             "r" #'lsp-rename))))

;; ----------------------------
;; Typescript Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("t" . "typescript")
                                 :desc "Format buffer"      "f" #'lsp-format-buffer
                                 :desc "Go to definition"   "d" #'lsp-find-definition
                                 :desc "Find references"    "r" #'lsp-find-references
                                 :desc "Rename symbol"      "n" #'lsp-rename
                                 :desc "Show errors"        "e" #'flycheck-list-errors
                                 :desc "Run npm script"     "s" #'npm-run-current-project
                                 :desc "Run dev server"     "d" (cmd! (npm-run-current-project "dev"))
                                 :desc "Build project"      "b" (cmd! (npm-run-current-project "build"))
                                 :desc "Test project"       "t" (cmd! (npm-run-current-project "test"))))))

;; ----------------------------
;; SH Keybindings
;; ----------------------------
(map! :map sh-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix ("e" . "execute")
                                 :desc "Run buffer" "r" #'+sh/run-current-buffer)
                                
                                (:prefix ("g" . "goto")
                                 :desc "Go to definition" "d" #'lsp-find-definition)
                                
                                (:prefix ("h" . "help")
                                 :desc "Documentation" "h" #'lsp-ui-doc-show)
                                
                                (:prefix ("l" . "lsp")
                                 :desc "Format buffer" "f" #'lsp-format-buffer
                                 :desc "Lint buffer" "l" #'lsp-execute-code-action
                                 :desc "Rename" "r" #'lsp-rename))))

;; ----------------------------
;; Shell Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("t" . "terminal")

                                 :desc "Open Zsh"  "z" (lambda () (interactive) 
                                                         (let ((shell-file-name "/bin/zsh"))
                                                           (open-shell-in shell-file-name)))
                                 :desc "Open Bash" "b" (lambda () (interactive) 
                                                         (let ((shell-file-name "/bin/bash"))
                                                           (open-shell-in shell-file-name)))
                                 :desc "Open Fish" "f" (lambda () (interactive) 
                                                         (let ((shell-file-name "/bin/fish"))
                                                           (open-shell-in shell-file-name)))))))

;; ----------------------------
;; Ruby Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("r" . "ruby")
                                 :desc "Run current file" "r" #'ruby-run-file
                                 :desc "Run Rails server" "s" #'rails-server
                                 :desc "Go to definition" "d" #'robe-jump
                                 :desc "Run tests" "t" #'rspec-run-single-file))))

;; ----------------------------
;; REST Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("r" . "rest")
                                 :desc "Send request at point" "s" #'restclient-http-send-current
                                 :desc "View last response" "v" #'restclient-show-response))))

;; ----------------------------
;; Plantuml Keybindings
;; ----------------------------
(map! :after plantuml-mode
      :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("u" . "uml")
                                 :desc "Preview diagram" "p" #'plantuml-preview
                                 :desc "Export diagram" "e" #'plantuml-export-buffer))))

;; ----------------------------
;; PHP Keybindings
;; ----------------------------
(map! :after php-mode
      :map php-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix ("p" . "php")
                                 :desc "Run PHP script" "r" #'php-execute-file
                                 :desc "Format buffer" "f" #'lsp-format-buffer
                                 :desc "Format with PHP-CS-Fixer" "F" #'php-cs-fixer-fix
                                 :desc "Go to definition" "d" #'lsp-find-definition
                                 :desc "Find references" "r" #'lsp-find-references
                                 :desc "Show documentation" "h" #'lsp-ui-doc-show)
                                
                                (:prefix ("t" . "tests")
                                 :desc "Run test at point" "t" #'phpunit-current-test
                                 :desc "Run current class tests" "c" #'phpunit-current-class
                                 :desc "Run current project tests" "p" #'phpunit-current-project))))

;; ----------------------------
;; Leader keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("o" . "org")
                                 :desc "Open agenda" "a" #'org-agenda
                                 :desc "Capture note" "c" #'org-capture
                                 :desc "Roam buffer" "r" #'org-roam-buffer-toggle
                                 :desc "Export to Hugo" "h" #'org-hugo-export-wim-to-md))))

;; ----------------------------
;; C/C++ Keybindings
;; ----------------------------
(map! :map (c-mode-map c++-mode-map)
      :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("c" . "C/C++")
                                 :desc "Compile project" "c" #'compile
                                 :desc "Run debugger" "d" #'gdb
                                 :desc "Switch header/source" "s" #'ff-find-other-file
                                 :desc "Format buffer" "f" #'lsp-format-buffer
                                 :desc "Format region" "r" #'lsp-format-region))))

;; ----------------------------
;; Lisp Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("l" . "Lisp")
                                 :desc "Start REPL"         "r" #'run-lisp
                                 :desc "Start SLIME"        "s" #'slime
                                 :desc "Eval last sexp"     "e" #'eval-last-sexp
                                 :desc "Eval buffer"        "b" #'eval-buffer
                                 :desc "Eval region"        "v" #'eval-region
                                 :desc "Expand macro"       "m" #'slime-macroexpand-1
                                 :desc "Find definition"    "g" #'slime-edit-definition
                                 :desc "SLIME selector"     "." #'slime-selector
                                 :desc "SLIME documentation" "d" #'slime-describe-symbol))))

;; ----------------------------
;; CSV Keybindings
;; ----------------------------
(map! :map csv-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("c" . "csv")
                                             "a" #'csv-align-fields
                                             "s" #'csv-sort-fields
                                             "t" #'csv-transpose
                                             "k" #'csv-kill-fields))))

;; ----------------------------
;; XML Keybindings
;; ----------------------------
(map! :map nxml-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("x" . "xml")
                                             "f" #'nxml-forward-element
                                             "b" #'nxml-backward-element
                                             "p" #'nxml-backward-up-element
                                             "n" #'nxml-down-element))))

;; ----------------------------
;; JSON Keybindings
;; ----------------------------
(map! :map json-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("j" . "json")
                                             "f" #'jq-format-json))))


;; ----------------------------
;; EJC Keybindings
;; ----------------------------
(map! :map ejc-sql-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("e" . "ejc")
                                             "c" #'ejc-connect
                                             "r" #'ejc-reconnect
                                             "x" #'ejc-execute-query
                                             "h" #'ejc-describe-table
                                             "d" #'ejc-describe-entity
                                             "t" #'ejc-show-tables-list))))

;; ----------------------------
;; Lisp Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("i" . "elisp")
                                 :desc "Evaluate buffer"       "b" #'eval-buffer
                                 :desc "Evaluate expression"   "e" #'eval-last-sexp
                                 :desc "Evaluate region"       "r" #'eval-region
                                 :desc "Describe function"     "f" #'describe-function
                                 :desc "Describe variable"     "v" #'describe-variable
                                 :desc "Find definition"       "g" #'xref-find-definitions
                                 :desc "Format region/buffer"  "=" #'indent-region))))

;; ----------------------------
;; GraphQL Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("q" . "graphql")
                                 :desc "Execute query"       "e" (if (fboundp 'graphql-run-query)
                                                                     #'graphql-run-query
                                                                   #'graphql-send-query)
                                 :desc "Jump to definition"  "d" #'lsp-find-definition
                                 :desc "Hover docs"          "h" #'lsp-hover
                                 :desc "Format buffer"       "f" #'lsp-format-buffer))))

;; ----------------------------
;; Javascript Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("j" . "javascript")
                                 :desc "Run node REPL" "r" #'nodejs-repl
                                 :desc "Send buffer to REPL" "b" #'nodejs-repl-send-buffer
                                 :desc "Send region to REPL" "s" #'nodejs-repl-send-region
                                 :desc "Format buffer" "f" #'prettier-js
                                 :desc "Go to definition" "d" #'lsp-find-definition
                                 :desc "Find references" "R" #'lsp-find-references
                                 :desc "Organize imports" "o" #'lsp-organize-imports))))

(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("p" . "project")
                                 :desc "npm test" "jt" (cmd! (compile "npm test"))
                                 :desc "npm start" "js" (cmd! (compile "npm start"))
                                 :desc "npm install" "ji" (cmd! (compile "npm install"))
                                 :desc "npm build" "jb" (cmd! (compile "npm run build"))))))

;; ----------------------------
;; Java Keybindings
;; ----------------------------
(map! :after java-mode
      :map java-mode-map
      :localleader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix ("j" . "java")
                                 :desc "Compile project" "c" #'lsp-java-build-project
                                 :desc "Run main class" "r" #'lsp-java-run-main
                                 :desc "Run debugger"   "d" #'dap-debug
                                 :desc "Run tests"      "t" #'lsp-java-run-test
                                 :desc "Organize imports" "o" #'lsp-java-organize-imports
                                 :desc "Extract to method" "m" #'lsp-java-extract-method
                                 :desc "Generate getters/setters" "g" #'lsp-java-generate-getters-and-setters)
                                (:prefix ("r" . "refactor")
                                 :desc "Code actions" "a" #'lsp-execute-code-action
                                 :desc "Rename" "r" #'lsp-rename))))

;; ----------------------------
;; Json Keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("o" . "json")
                                 :desc "Format buffer"   "f" (lambda () (interactive)
                                                               (if (region-active-p)
                                                                   (json-reformat-region (region-beginning) (region-end))
                                                                 (json-reformat-region (point-min) (point-max))))
                                 :desc "Validate buffer" "v" #'lsp-format-buffer))))


;; ----------------------------
;; Latex keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("l" . "latex")
                                 :desc "Compile LaTeX"    "c" #'TeX-command-master
                                 :desc "View PDF"         "v" #'TeX-view
                                 :desc "Clean aux files"  "x" #'TeX-clean))))

;; ----------------------------
;; Lua keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("u" . "lua")
                                 :desc "Evaluate buffer" "b" #'lua-send-buffer
                                 :desc "Evaluate region" "r" #'lua-send-region
                                 :desc "Run REPL" "s" #'lua-start-process))))

;; ----------------------------
;; Markdown keybindings
;; ----------------------------
(map! :leader
      (:prefix-map ("e" . "editor")
                   (:prefix-map ("l" . "language")
                                (:prefix-map ("r" . "markdown")
                                 :desc "Preview file" "p" #'markdown-live-preview-mode
                                 :desc "Export to PDF" "f" #'markdown-export-to-html-and-pdf
                                 :desc "Export to HTML" "h" #'markdown-export-to-html))))


(provide 'keybindings)

;;; keybindings.el ends here

