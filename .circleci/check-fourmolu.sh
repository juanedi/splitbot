#!/usr/bin/env bash
set -euo pipefail

# format all files!
find app src -name '*.hs' -exec fourmolu -i {} + 2> /dev/null

diff_file=$(mktemp)

git diff > "$diff_file"

if [ -s "$diff_file" ]; then
    echo "Formatting error detected! Here's the diff with the changes after applying fourmolu:"
    echo
    cat "$diff_file"
    exit 1
else
    echo "Formatter check OK!"
    exit 0
fi
