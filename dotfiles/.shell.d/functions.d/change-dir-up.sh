#!/usr/bin/env sh
#
# NAVIGATION
# Description: Change several directories up
# Example: up 3
#

cd_up() {

    local LIMIT="$1"
    local LOCATION=""

    # If ${LIMIT} is empty or does not contains integer or is les than zero.
    # Then return error message.
    if [ -z "${LIMIT}" ] || ! printf '%s' "${LIMIT}" | grep -qE '^[0-9]+$' || [ "${LIMIT}" -le 0 ]; then
        echo "Error: Please provide a positive number." >&2
        return 1
    else
        for count in $(seq 1 "${LIMIT}"); do
            LOCATION="../${LOCATION}"
        done
    fi

    if ! cd "${LOCATION}"; then
        echo "Error: Could not go up ${LIMIT} directories." >&2
        return 1
    fi

    return 0
}
