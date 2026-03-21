#!/usr/bin/env sh
#
# SHELL DETECTION
# Description: Detect current shell and expose helper functions/variables
#

# Detect current shell
if [ -n "$ZSH_VERSION" ]; then
    export CURRENT_SHELL="zsh"
elif [ -n "$BASH_VERSION" ]; then
    export CURRENT_SHELL="bash"
elif [ -n "$FISH_VERSION" ]; then
    export CURRENT_SHELL="fish"
else
    export CURRENT_SHELL="sh"
fi

# Helper: is_zsh, is_bash, is_fish
is_zsh()  { [ "$CURRENT_SHELL" = "zsh"  ]; }
is_bash() { [ "$CURRENT_SHELL" = "bash" ]; }
is_fish() { [ "$CURRENT_SHELL" = "fish" ]; }
