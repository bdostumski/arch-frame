#!/usr/bin/env bash
#
# BASH INITIALIZATION
# Description: Bootstrap bash-completion and create required directories
#

# Logs directory
LOGS="${HOME}/.bashrc.d/.logs"
[ ! -d "${LOGS}" ] && mkdir -p "${LOGS}"

# History file
[ ! -f "${LOGS}/.bash_history" ] && touch "${LOGS}/.bash_history"

# bash-completion (Arch Linux path)
if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi
