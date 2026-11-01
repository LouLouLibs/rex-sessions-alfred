<div align="center">

<img src="rex-sessions/icon.png" width="96" alt="Rex Sessions icon">

<h1>rex-sessions-alfred</h1>
<h3>Fuzzy-pick any Rex tab, on this Mac or on your servers, from Alfred, with a Quick Look preview of its screen</h3>

[![Vibecoded](https://img.shields.io/badge/vibecoded-%E2%9C%A8-blueviolet)](#a-word-on-vibe-coding)
[![OCaml](https://img.shields.io/badge/OCaml-5.4-EC6813?logo=ocaml&logoColor=white)](https://ocaml.org)
[![macOS](https://img.shields.io/badge/macOS-Apple%20Silicon-000000?logo=apple&logoColor=white)](#install)
[![Release](https://github.com/LouLouLibs/rex-sessions-alfred/actions/workflows/release.yml/badge.svg)](https://github.com/LouLouLibs/rex-sessions-alfred/releases/latest)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

</div>

***

[**Install**](#install) · [**Use**](#use) · [**Settings**](#settings) · [**How it works**](#how-it-works) · [**Development**](#development) · [**Vibe coding**](#a-word-on-vibe-coding)

***

I keep a lot of [Rex](https://superlogical.com) sessions open, some on this Mac and
some on remote hosts, and I wanted to jump to any tab the way I open apps: type a few
letters in [Alfred](https://www.alfredapp.com), hit return. Type `rx` and a query, and
every tab of every session on every host Rex knows about (this Mac and each server in
`rex hosts`) is ranked as you type. Each row shows Rex's own icon for the program
running in the tab, and <kbd>⇧</kbd> opens a Quick Look snapshot of the tab's screen.
Inspired by [resh](https://github.com/CruelAddict/resh).

It's a single OCaml binary plus a small Lua script that Rex runs. Results come from a
cache, so the picker answers instantly even though remote hosts take seconds to
snapshot.

## Install

Download `Rex-Sessions.alfredworkflow` from the
[latest release](https://github.com/LouLouLibs/rex-sessions-alfred/releases/latest)
and open it.

Requirements:

- Alfred 5 with the Powerpack
- Rex, on an Apple-silicon Mac. The release is arm64 only, because Rex runs only on
  Apple silicon.

The binaries aren't notarized. If macOS blocks them, clear the quarantine flag on the
installed workflow folder. You can find that folder by right-clicking the workflow in
Alfred and choosing *Open in Finder*:

```sh
xattr -dr com.apple.quarantine /path/to/the/workflow/folder
```

## Use

Type `rx` in Alfred, then an optional query. The query matches the session name, tab
name, terminal title, directory, host and running programs. Every word must match, and
an exact session or tab name ranks first.

| Key | Action |
| --- | --- |
| <kbd>↵</kbd> | Jump to the tab in Rex (remote tabs too) |
| <kbd>⇧</kbd> / <kbd>⌘Y</kbd> | Quick Look preview of the tab's screen |
| <kbd>⌘↵</kbd> | Close the tab (asks first) |
| <kbd>⌥↵</kbd> | Copy the session and tab IDs |
| <kbd>↵</kbd> on the last row | Create a local session named after the query |

## Settings

These are workflow variables you set in Alfred's workflow configuration:

| Variable | Default | Meaning |
| --- | --- | --- |
| `refresh_seconds` | `5` | Cache age that triggers a background refresh |
| `preview_lines` | `70` | Lines of scrollback in each preview |
| `icon_aliases` | `vm-launcher=claude` | Extra `program=icon` pairs, comma-separated |
| `rex_path` | blank | Path to the `rex` CLI (blank uses Rex's bundled copy) |
| `rex_app` | blank | Path to Rex.app for icons (blank searches `/Applications`) |

Icon names are Rex's glyph names (`claude`, `neovim`, `python`, `terminal`, …). A
program with no matching glyph gets the generic terminal icon.

## How it works

`rex_sessions_alfred` has three modes: `filter` (the Script Filter), `refresh` and
`run` (the actions).

- **Snapshots.** A refresh runs `snapshot.lua` through `rex do` on every host at
  once. In one connection it collects each tab's title, directory, panes, foreground
  programs and screen. Each host is written to `index.json`, with one HTML preview per
  tab, under `~/Library/Caches/rex-sessions-alfred` as soon as it answers. A slow or
  unreachable host never holds back the others, and an unreachable host keeps its last
  known tabs.
- **The picker** reads only that cache. When the cache is stale, it starts a detached
  refresh and asks Alfred to rerun until the refresh finishes. A lock that records the
  refresh's PID keeps two refreshes from overlapping.
- **Icons.** The workflow ships no Rex artwork. On first run, the bundled
  `extract_rex_icons` helper (Swift) renders Rex's program glyphs from your installed
  Rex app into the cache, and renders them again after Rex updates.

## Development

You need OCaml 5 with `dune` and `yojson` (for example through opam), plus the Xcode
command-line tools for `swiftc`.

```sh
./build.sh   # builds, runs the tests, writes dist/Rex-Sessions.alfredworkflow
```

Open `dist/Rex-Sessions.alfredworkflow` to install your build.

To release, set `version` in `rex-sessions/info.plist`, commit, and push a matching
tag (`git tag v0.2.0 && git push origin v0.2.0`). The Release action builds the bundle
on Apple silicon and publishes it.

Layout:

- `lib/` holds the library:
  - picker and ranking: `picker.ml`, `fuzzy_match.ml`
  - snapshots: `refresh.ml`, plus the cache in `cache_index.ml` and the lock in
    `refresh_lock.ml`
  - actions: `tab_action.ml`
  - the concurrent process runner: `process.ml`
  - icon matching: `program_icon.ml`
- `bin/` is the entry point.
- `rex-sessions/` holds the workflow's `info.plist`, `snapshot.lua` and icon.
- `tools/` holds the Swift icon helpers.

## A word on vibe coding

This project is **totally vibe coded**. Every line of code and every test was written
by an AI coding agent ([Claude Code](https://claude.com/claude-code)). My part was
describing what I wanted, using it in Alfred, and pushing back when something felt
off. An independent agent review then fed a round of fixes.

It has unit tests, and it has been checked against real local and remote Rex hosts.
Still, treat it like software from an enthusiastic, fast, occasionally overconfident
collaborator, and open an issue when something looks wrong. The worst it can do is
close a tab, and it asks before doing that.

## License

MIT. Rex is a product of Superlogical; this workflow is an independent project and not
affiliated with it.

***

<div align="center"><sub>Part of <a href="https://github.com/LouLouLibs">LouLouLibs</a></sub></div>
