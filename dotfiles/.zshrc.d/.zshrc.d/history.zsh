#!/usr/bin/env zsh
#
# HISTORY
# Description: setup history options
#

# -----------------
# HISTORY CONFIGURATION
# -----------------

# Max number of lines in history file (in memory)
HISTSIZE=50000
# Max number of lines saved in history file (on disk)
SAVEHIST=50000
# Location of the history file
HISTFILE="${HOME}/.zsh.d/.logs/.zsh_history"

# History options
setopt appendhistory          # Append history instead of overwriting
setopt sharehistory           # Share history across sessions
setopt inc_append_history     # Write commands to history immediately
setopt hist_ignore_dups       # Ignore duplicate commands
setopt hist_find_no_dups      # Do not show duplicates when searching history
setopt hist_ignore_space      # Ignore commands starting with a space
setopt hist_verify            # Show command before execution
setopt hist_expire_dups_first # Expire duplicate entries first
setopt hist_reduce_blanks     # Remove superfluous blanks
