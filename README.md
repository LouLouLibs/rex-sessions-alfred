# Rex Sessions for Alfred

An [Alfred](https://www.alfredapp.com) workflow that fuzzy-picks tabs in
Rex (by [Superlogical](https://superlogical.com)), the terminal multiplexer, across every host Rex
knows about: this Mac and each server in `rex hosts`. Inspired by
[resh](https://github.com/CruelAddict/resh).

- Lists every tab of every session, local and remote.
- Each row shows Rex's own icon for the program running in the tab's focused pane.
- Press ⇧ or ⌘Y on a row to see a Quick Look snapshot of the tab's screen.
- Answers instantly from a cache. Remote hosts take a few seconds to snapshot, so
  they refresh in the background.

## Install

Download `Rex-Sessions.alfredworkflow` from the
[latest release](https://github.com/LouLouLibs/rex-sessions-alfred/releases/latest)
and open it. You need Alfred 5 with the Powerpack, and Rex on an Apple-silicon Mac.
The release is built for arm64 only, because Rex runs only on Apple silicon.

The binaries aren't notarized. If macOS blocks them, clear the quarantine flag:

```sh
xattr -dr com.apple.quarantine ~/Library/Application\ Support/Alfred/Alfred.alfredpreferences/workflows/*/rex_sessions_alfred
```

## Use

Type `rx` in Alfred, then an optional query. The query matches the session name, tab
name, terminal title, directory, host and running programs, and every word must
match.

| Key | Action |
| --- | --- |
| ↵ | Jump to the tab in Rex (remote tabs too) |
| ⇧ / ⌘Y | Quick Look preview of the tab's screen |
| ⌘↵ | Close the tab (asks first) |
| ⌥↵ | Copy the session and tab IDs |
| ↵ on the last row | Create a local session named after the query |

## Settings

These are workflow variables you set in Alfred's workflow configuration:

| Variable | Default | Meaning |
| --- | --- | --- |
| `refresh_seconds` | `5` | Cache age that triggers a background refresh |
| `preview_lines` | `70` | Lines of scrollback in each preview |
| `icon_aliases` | `vm-launcher=claude` | Extra `program=icon` pairs, comma-separated |
| `rex_path` | blank | Path to the `rex` CLI (blank uses Rex's bundled copy) |
| `rex_app` | blank | Path to Rex.app for icons (blank searches `/Applications`) |

## How it works

`rex_sessions_alfred` is a single OCaml binary.

- **Snapshots.** A refresh runs `snapshot.lua` through `rex do` on each host, in
  parallel. In one connection it collects each tab's title, directory, panes,
  foreground programs and screen. It writes `index.json` and one HTML preview per tab
  under `~/Library/Caches/rex-sessions-alfred`, publishing each host's results as
  they arrive.
- **The picker.** It reads only that cache. When the cache is stale it starts a
  detached refresh and asks Alfred to rerun until the refresh finishes.
- **Icons.** The workflow ships no Rex artwork. On first run, the bundled
  `extract_rex_icons` helper (Swift) renders Rex's program glyphs from your installed
  Rex app into the cache, and renders them again whenever Rex's asset catalog
  changes.

## Build

You need OCaml 5 with `dune`, `yojson` and `str` (for example through opam), plus the
Xcode command-line tools for `swiftc`.

```sh
./build.sh   # builds, runs the tests, writes dist/Rex-Sessions.alfredworkflow
```

To release, set `version` in `rex-sessions/info.plist`, commit, and push a matching tag
(`git tag v0.2.0 && git push origin v0.2.0`). The Release action builds the bundle on
Apple silicon and publishes it.

## License

MIT
