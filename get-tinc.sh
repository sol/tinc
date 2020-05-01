#!/bin/bash
set -o nounset
set -o errexit

dst="$HOME/.tinc/bin"
tinc="$dst/tinc"

mkdir -p "$dst"

os="${1:-${TRAVIS_OS_NAME:-linux}}"
url=$(curl --fail --silent --show-error https://api.github.com/repos/sol/tinc/releases/latest | jq -r ".assets[] | select(.name | test(\"$os\")) | .browser_download_url")

echo "Downloading $url"

curl --fail --silent --show-error --location "$url" | gunzip > "$tinc.tmp"
chmod +x "$tinc.tmp"
mv "$tinc.tmp" "$tinc"

echo "Installed tinc to $tinc"
