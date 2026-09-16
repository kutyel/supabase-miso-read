# Read the Bible 📖 — Miso + Supabase

[![NixCI](https://nix-ci.com/badge/gh:kutyel:supabase-miso-read)](https://nix-ci.com/gh:kutyel:supabase-miso-read)

A Bible reading tracker: pick a book, chapter and date, hit **Read**, and watch
your yearly calendar heatmap fill up. This is a Haskell port of
[read-the-bible-svelte](https://github.com/kutyel/read-the-bible-svelte)
(Svelte + Firebase), rebuilt with:

- [miso](https://github.com/dmjio/miso) `1.14.0`, compiled to WebAssembly
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

### Custom domain: read-the-bible-in-the.cloud

The app stays hosted on GitHub Pages; Hostinger manages the domain and DNS.

1. In [the repository's Pages settings](https://github.com/kutyel/supabase-miso-read/settings/pages),
   set **Custom domain** to `read-the-bible-in-the.cloud` and save it before
   changing DNS. This project deploys through GitHub Actions, so GitHub's Pages
   setting is authoritative; a `CNAME` file in the artifact is not required
   and does not configure the domain.
2. In Hostinger, open **Domains → DNS**, select `read-the-bible-in-the.cloud`,
   and open **DNS / Nameservers → DNS records**. If the domain uses another
   provider's nameservers, make these changes at that provider instead.
   Replace conflicting website records for `@` and `www` with these records
   (use the default TTL). Leave email records such as MX and TXT in place.

   | Type | Name | Target |
   | ---- | ---- | ------ |
   | A | @ | 185.199.108.153 |
   | A | @ | 185.199.109.153 |
   | A | @ | 185.199.110.153 |
   | A | @ | 185.199.111.153 |
   | CNAME | www | kutyel.github.io |

   If using IPv6, use all four GitHub Pages AAAA records for `@`:
   `2606:50c0:8000::153`, `2606:50c0:8001::153`,
   `2606:50c0:8002::153`, and `2606:50c0:8003::153`.
   Remove any old AAAA records pointing to the previous host even if you
   choose to use only the A records. The `www` target is a hostname,
   without `https://` or `/supabase-miso-read/`.
3. In Supabase **Authentication → URL Configuration**, set **Site URL** to
   `https://read-the-bible-in-the.cloud/` and add that exact URL to
   **Redirect URLs**. Keep `http://localhost:8080/` for local development
   and the old GitHub Pages URL during the transition. The Google sign-in
   helper already redirects to the current origin and path; no code change
   is needed. Google's authorized callback remains the Supabase callback
   `https://ljknwlqyxougfijkyybq.supabase.co/auth/v1/callback`.
4. Allow DNS to propagate (up to 24 hours), then enable **Enforce HTTPS**
   in GitHub Pages once the certificate is ready (this can also take up to
   24 hours). Open `https://read-the-bible-in-the.cloud/` and check sign-in
   and recording a reading. GitHub Pages redirects the old project URL to
   the custom domain, and redirects `www` to the chosen apex domain.

All app assets use relative URLs, so the same bundle works at the domain's
root without a `/supabase-miso-read/` prefix.

References: [GitHub Pages custom domains](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site),
[Hostinger DNS management](https://www.hostinger.com/support/1583249-how-to-manage-dns-records-at-hostinger/),
[Supabase redirect URLs](https://supabase.com/docs/guides/auth/redirect-urls).
