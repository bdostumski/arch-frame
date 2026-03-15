#!/usr/bin/env bash
#
# BASH HISTORY
# Description: Bash history settings (equivalent of zsh history.zsh)
#

HISTSIZE=50000
HISTFILESIZE=50000
HISTFILE="${HOME}/.bashrc.d/.logs/.bash_history"

HISTCONTROL=ignoreboth:erasedups  # ignore duplicates and lines starting with space
HISTIGNORE="ls:pwd:exit:sudo reboot:cd:cd -:cd .."

shopt -s histappend       # append to history file, don't overwrite
shopt -s cmdhist          # save multi-line commands as single history entry
shopt -s histreedit       # allow re-editing failed history substitution
shopt -s histverify       # show command before execution (like zsh hist_verify)

# Sync history across terminals
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
