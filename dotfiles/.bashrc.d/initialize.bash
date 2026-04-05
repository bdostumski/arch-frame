#!/usr/bin/env bash
#
# BASH INITIALIZATION
# Description: Bootstrap bash-completion, blesh, and create required directories
#

# -----------------
# LOGS CONFIGURATION
# -----------------
LOGS="${HOME}/.bashrc.d/.logs"
# Create .logs to store application messages
[[ ! -d "${LOGS}" ]] && mkdir -p "${LOGS}"

# -----------------
# HISTORY FILE
# -----------------
# Create history file if it does not exist
[[ ! -f "${LOGS}/.bash_history" ]] && touch "${LOGS}/.bash_history"

# -----------------
# BASH COMPLETION
# -----------------
# System-level bash-completion (Arch Linux paths)
if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    source /usr/share/bash-completion/bash_completion
elif [[ -f /etc/bash_completion ]]; then
    source /etc/bash_completion
fi

# User-level bash completions (drop-in directory)
if [[ -d "${HOME}/.bashrc.d/completions" ]]; then
    for _comp in "${HOME}/.bashrc.d/completions/"*.bash; do
        [[ -f "${_comp}" ]] && source "${_comp}"
    done
    unset _comp
fi

# -----------------
# READLINE / AUTOSUGGESTION CONFIGURATION
# -----------------
# Equivalent of zsh's ZSH_AUTOSUGGEST_STRATEGY — bash uses readline settings
bind 'set completion-ignore-case on'       # case-insensitive completion (like zsh matcher-list)
bind 'set show-all-if-ambiguous on'        # show completions immediately on tab
bind 'set show-all-if-unmodified on'       # show completions if no partial match
bind 'set mark-symlinked-directories on'   # add trailing slash to symlinked dirs
bind 'set visible-stats on'                # show file type indicators in completion list
bind 'set colored-stats on'                # colorize completion list (uses LS_COLORS)
bind 'set colored-completion-prefix on'    # colorize the common prefix in completions
bind 'set menu-complete-display-prefix on' # show prefix before cycling completions
bind 'set skip-completed-text on'          # avoid duplicating existing text on completion

# -----------------
# BLE.SH — Bash Line Editor (autosuggestions + syntax highlighting for bash)
# Equivalent of zinit loading zsh-autosuggestions + zsh-syntax-highlighting
# Install: git clone --recursive https://github.com/akinomyoga/ble.sh.git
#          make -C ble.sh install PREFIX=~/.local
# -----------------
BLESH="${HOME}/.local/share/blesh/ble.sh"
if [[ -f "${BLESH}" ]]; then
    source "${BLESH}" --noattach
fi
unset BLESH
