#!/usr/bin/env zsh
#
# NAVIGATION
# Description: Change several directories up
# Example: up 3
#

function cd_up() {

	local LIMIT="$1"
	local LOCATION=""

	# If ${LIMIT} is empty or does not contains integer or is les than zero.
	# Then return error message.
	if [[ -z "${LIMIT}" || ! "${LIMIT}" =~ ^[0-9]+$ || "${LIMIT}" -le 0 ]]; then
		echo "Error: Please provide a positive number." >&2
		return 1
	else
		LIMIT=1
		for ((count = 1; count <= LIMIT; count++)); do
			LOCATION="../${LOCATION}"
		done
	fi

	if ! cd "${LOCATION}"; then
		echo "Error: Could not go up ${LIMIT} directories." >&2
		return 1
	fi
}
