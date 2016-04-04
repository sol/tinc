#!/bin/sh
set -o nounset
set -o errexit

dst="$HOME/.tinc/bin"
tinc="$dst/tinc"

mkdir -p "$dst"
curl https://zalora-public.s3.amazonaws.com/tinc > "$tinc.tmp"
chmod +x "$tinc.tmp"
mv "$tinc.tmp" "$tinc"

echo "Installed tinc to $tinc."
