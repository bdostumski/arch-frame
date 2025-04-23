#!/usr/bin/env zsh
#
# HISTORY
# Description: setup history options
#

# -----------------
# HISTORY CONFIGURATION
# -----------------
# Max number of lines in history file
HISTSIZE=50000
# ----------
# History options
setopt appendhistory      # append history insted of overwriting
setopt sharehistory       # share history across sessions
setopt inc_append_history # write commands to history imediately
setopt hist_ignore_dups   # ignore duplicate commands
setopt hist_find_no_dups  # do not display duplicates when searching history
setopt hist_ignore_space  # ignore commands starting with space
setopt hist_verify        # show command before execution
