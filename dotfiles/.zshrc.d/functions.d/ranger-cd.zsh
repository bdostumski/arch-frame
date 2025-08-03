#!/usr/bin/env zsh
#
# Ranger cd function
# Description:  automatically change the current working directory to the last visited one after ranger quits.
# To undo the effect of this function, you can type "cd -" to return to the original directory.
#

function ranger_cd() {
    TEMP_FILE="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
    ranger --choosedir="${TEMP_FILE}" -- "${@:-$PWD}"

    if CHOSEN_DIR="$(cat -- "${TEMP_FILE}")" && [ -n "${CHOSEN_DIR}" ] && [ "${CHOSEN_DIR}" != "${PWD}" ]; then
        cd -- "${CHOSEN_DIR}"
    fi

    rm -f -- "${TEMP_FILE}"

    return 0
}
