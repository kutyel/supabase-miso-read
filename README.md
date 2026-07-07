# Read the Bible 📖 — Miso + Supabase

[![NixCI](https://nix-ci.com/badge/gh:kutyel:supabase-miso-read)](https://nix-ci.com/gh:kutyel:supabase-miso-read)

A Bible reading tracker: pick a book, chapter and date, hit **Read**, and watch
your yearly calendar heatmap fill up. This is a Haskell port of
[read-the-bible-svelte](https://github.com/kutyel/read-the-bible-svelte)
(Svelte + Firebase), rebuilt with:

- [miso](https://github.com/dmjio/miso) `1.11.0`, compiled to WebAssembly
- [supabase-miso](https://github.com/haskell-miso/supabase-miso) for auth + database
- [Google Charts](https://developers.google.com/chart/interactive/docs/gallery/calendar)
  calendar, driven through Miso's JS FFI (`src/Interop.hs` + the helpers in
  `static/index.html`)

## Features

- Sign in with Google (OAuth) or email + password (Supabase Auth)
- Session restore on page load
- Readings stored per-user in the `readings` table, guarded by row-level security
- Yearly calendar heatmap of readings with HTML tooltips, per selected year
- Undo the last recorded reading

## Setup

- Install [Nix](https://nixos.org/download) with flakes enabled
- Install [Cachix](https://docs.cachix.org/installation) and use miso's cache
  (highly recommended, avoids building GHC):

```sh
cachix use haskell-miso-cachix
```

### Supabase

The app talks to the Supabase project configured at the top of
`static/index.html` (URL + publishable key). One-time database setup: run
`supabase/schema.sql` in the Supabase SQL editor. It adds the `date` column to
the existing `readings` table, defaults `"user"` to `auth.uid()`, and installs
the row-level-security policies.

For Google sign-in the Google provider must be enabled under
*Authentication → Providers*, and your app origin (e.g.
`http://localhost:8080`) added to *Authentication → URL Configuration →
Redirect URLs*.

## Build and run (wasm)

```sh
nix develop .#wasm --command bash -c "make && make serve"
```

or, for iterating:

```sh
nix develop .#wasm
make build && make serve
```

Then open http://localhost:8080.

## Project layout

| Path                  | Purpose                                                           |
| --------------------- | ----------------------------------------------------------------- |
| `src/Main.hs`         | Model / update / view (The Elm Architecture)                      |
| `src/Bible.hs`        | The 66 books and their chapter counts                             |
| `src/Interop.hs`      | Supabase + Google Charts FFI (auth, insert-returning, calendar)   |
| `static/index.html`   | Supabase client init, supabase-miso JS glue, chart helpers, CSS   |
| `static/index.js`     | WASI shim that instantiates and starts the compiled `app.wasm`    |
| `supabase/schema.sql` | Idempotent DB migration: `date` column, defaults, RLS policies    |

## CI / deployment

`.github/workflows/main.yml` builds the wasm bundle with Nix and deploys
`public/` to GitHub Pages on every push to `main`.
