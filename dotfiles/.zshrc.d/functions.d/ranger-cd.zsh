#!/usr/bin/env zsh
#
# Ranger cd function
# Description:  automatically change the current working directory to the last visited one after ranger quits. 
# To undo the effect of this function, you can type "cd -" to return to the original directory.
#

ranger_cd() {
    temp_file="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
    ranger --choosedir="$temp_file" -- "${@:-$PWD}"
    if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
        cd -- "$chosen_dir"
    fi
    rm -f -- "$temp_file"
}
