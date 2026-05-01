#!/usr/bin/env sh
#
# LOG AUTHOR
# Description: Show git log filtered by author
# Usage: logauthor <author>
#

git_logauthor() {
    git log --graph --decorate --full-history --author="${1}" | bat --paging=always --language=gitlog
    return 0
}
