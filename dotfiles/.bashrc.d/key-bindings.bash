#!/usr/bin/env bash
#
# BASH KEY BINDINGS
# Description: Bash equivalents of the zsh key-bindings.zsh bindings
#

# Use emacs readline bindings (default in bash)
set -o emacs

# Word navigation (Alt+u = backward-word, Alt+p = forward-word)
bind '"\eu": backward-word'
bind '"\ep": forward-word'

# Line navigation (Alt+n = beginning-of-line, Alt+m = end-of-line)
bind '"\en": beginning-of-line'
bind '"\em": end-of-line'

# Delete word before/after cursor
bind '"\ei": backward-kill-word'
bind '"\eo": kill-word'

# Delete char
bind '"\eh": backward-delete-char'
bind '"\ek": delete-char'

# History search
bind '"\e[": history-search-backward'
bind '"\e]": history-search-forward'

# FZF key bindings (loaded separately by fzf --bash in profile.bash)
