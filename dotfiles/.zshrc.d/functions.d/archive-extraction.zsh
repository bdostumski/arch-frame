#!/usr/bin/env zsh
#
# EXTRACT ARCHIVE FILES
# Usage: extract [FILE]
#

function extract() {
	if [[ $# -eq 0 ]]; then
		# Display usage if no parameters given
		echo "Usage: extract [FILE]..." >&2
		echo "" >&2
		echo "Extract one or more archive files based on their extension." >&2
		echo "" >&2
		echo "Supported formats:" >&2
		echo "  .tar       .tar.gz    .tar.bz2   .tar.xz    .tgz       .tbz2      .txz       .cbt" >&2
		echo "  .gz        .bz2       .lzma      .xz        .Z" >&2
		echo "  .zip       .cbz       .epub" >&2
		echo "  .rar       .cbr" >&2
		echo "  .7z        .cb7       .arj       .cab       .chm       .deb       .dmg       .iso" >&2
		echo "  .lzh       .msi       .pkg       .rpm       .udf       .wim       .xar" >&2
		echo "  .exe       .cpio      .ace       .cba" >&2
		echo "" >&2
		echo "Note: File format is determined by file extension." >&2
		return 1
	else
		for FILE in "$@"; do
			if [ -f "${FILE}" ]; then
				case "${FILE%,}" in
				*.cbt | *.tar.bz2 | *.tar.gz | *.tar.xz | *.tbz2 | *.tgz | *.txz | *.tar)
					tar xfv "${FILE}"
					;;
				*.lzma) unlzma ./"${FILE}" ;;
				*.bz2) bnzip2 ./"${FILE}" ;;
				*.cbr | *.rar) unrar x -ad ./"${FILE}" ;;
				*.gz) gunzip ./"${FILE}" ;;
				*.cbz | *.epub | *.zip) unzip ./"${FILE}" ;;
				*.z) uncompress ./"${FILE}" ;;
				*.7z | *.arj | *.cab | *.cb7 | *.chm | *.deb | *.dmg | *.iso | *.lzh | *.msi | *.pkg | *.rpm | *.udf | *.wim | *.xar)
					7z x ./"${FILE}"
					;;
				*.xz) unxz ./"${FILE}" ;;
				*.exe) cabextract ./"${FILE}" ;;
				*.cpio) cpio -id <./"${FILE}" ;;
				*.cba | *.ace) unace x ./"${FILE}" ;;
				*)
					echo "Extract: '${FILE}' - unknown archive method" >&2
					return 1
					;;
				esac
			else
				echo "'${FILE}' - file does not exists" >&2
				return 1
			fi
		done
	fi
}
