#!/usr/bin/env bash
#
# BASH HISTORY
# Description: Bash history settings — mirrors zsh history.zsh
#

# -----------------
# HISTORY CONFIGURATION
# -----------------

# Max number of commands kept in memory (equivalent of zsh HISTSIZE)
HISTSIZE=50000
# Max number of lines saved to disk (equivalent of zsh SAVEHIST)
HISTFILESIZE=50000
# Location of the history file (mirrors zsh HISTFILE path convention)
HISTFILE="${HOME}/.bashrc.d/.logs/.bash_history"

# ignorespace = ignore commands starting with a space (zsh: hist_ignore_space)
# ignoredups  = ignore consecutive duplicates     (zsh: hist_ignore_dups)
# erasedups   = remove ALL older duplicates        (zsh: hist_ignore_all_dups)
HISTCONTROL=ignorespace:ignoredups:erasedups

# Ignore trivial commands from history (zsh: ZSH_AUTOSUGGEST_HISTORY_IGNORE equivalent)
HISTIGNORE="ls:ll:la:lt:l:pwd:exit:cd:cd -:cd ..:sudo reboot:history:clear"

# History shopts
shopt -s histappend # append to history file, don't overwrite (zsh: appendhistory)
shopt -s cmdhist    # save multi-line commands as one entry
shopt -s histreedit # allow re-editing a failed history substitution
shopt -s histverify # show command before execution (zsh: hist_verify)

# -----------------
# SYNC HISTORY ACROSS TERMINALS
# Equivalent of zsh's sharehistory + inc_append_history behavior:
# After each command — flush to disk (history -a), clear memory (history -c),
# reload from disk (history -r) so all terminals share the same history.
# -----------------
# Safe PROMPT_COMMAND append (works for both string and unset cases)
_history_sync='history -a; history -c; history -r'
if [[ -z "${PROMPT_COMMAND}" ]]; then
    PROMPT_COMMAND="${_history_sync}"
else
    PROMPT_COMMAND="${PROMPT_COMMAND%;}; ${_history_sync}"
fi
unset _history_sync
