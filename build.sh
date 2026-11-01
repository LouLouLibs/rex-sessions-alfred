#!/bin/bash
# Build the OCaml binary and package it with the workflow assets in rex-sessions/
# into dist/Rex-Sessions.alfredworkflow (open that file to install or update).
set -euo pipefail
cd "$(dirname "$0")"
eval "$(opam env)"
dune build --profile release
dune test --profile release

staging=_build/workflow
rm -rf "$staging"
mkdir -p "$staging" dist
cp rex-sessions/info.plist rex-sessions/snapshot.lua rex-sessions/icon.png "$staging/"
# Renders Rex's program glyphs from the installed app at first run (no Rex artwork ships).
# Rex is Apple-silicon only, so the release is too.
xcrun swiftc -O -target arm64-apple-macos13 tools/extract_rex_icons.swift -o "$staging/extract_rex_icons"
install -m 755 _build/default/bin/rex_sessions_alfred.exe "$staging/rex_sessions_alfred"
plutil -lint -s "$staging/info.plist"

rm -f dist/Rex-Sessions.alfredworkflow
(cd "$staging" && zip -q -X -r ../../dist/Rex-Sessions.alfredworkflow info.plist snapshot.lua icon.png rex_sessions_alfred extract_rex_icons)
echo "built dist/Rex-Sessions.alfredworkflow"
