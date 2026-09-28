# Sirimbo repository map and contributor notes

This document is for fellow ChatGPT/Codex-style agents working in this repository. Use it as a quick reference before you make changes.

## Workflow expectations
- Do not create Git commits or change the Git index. Leave staging and commits to the maintainer.
- Preserve existing work, including unrelated changes in files you edit.
- Use the checked-in PNPM 10 workspaces (Node 24.x) for JavaScript/TypeScript tooling.
- After code changes, run the affected package's lint and typecheck commands, plus relevant tests. Report failures without changing unrelated code to fix them.
- Prefer incremental SQL migrations. For changes under `migrations/current/` or `migrations/fixtures/`, follow `migrations/current/AGENTS.md`.
- `schema/` is generated from the canonical `schema.sql` dump via `python schema/split.py < schema.sql`. Do not hand-edit files under `schema/`—regenerate from the dump instead.

## Running development services
- The maintainer often runs `overmind` with the root `Procfile`. It starts the web server, backend, migration watcher, and code generators among other services.
- Before starting a service, run `overmind status` from the repository root. If the command reports a permission error, retry with the required sandbox approval. A permission error does not mean that Overmind is stopped.
- If `web` is running, reuse it. Run `ss -ltnp` to find listening ports and their process IDs. Use `ps -eo pid,ppid,args` to match the Next.js process to the `web` process from `overmind status`. Request sandbox approval if these commands cannot see host processes or sockets. Use that port for browser requests and `PLAYWRIGHT_BASE_URL`. If the port remains unknown, ask the maintainer for the URL instead of starting another server.
- If Overmind is unavailable or `web` is stopped, use the same process and socket commands to find standalone Next.js servers. Start a server only after establishing that this checkout has none. Do not assume port 3000 or 5100.
- Do not start a second Next.js server in the same checkout. A different port still shares `frontend/.next/` and can cause conflicts. Before a build or cleanup of `.next/`, coordinate with the maintainer if the web server is running. Do not stop or restart maintainer-owned services without direction.
- Background watchers can apply migrations and regenerate files before a manual command runs. For migration verification, use the command in `migrations/current/AGENTS.md` even when the watcher is running. Do not infer success from a running process or changed generated files. Keep generated output intact, as described below.
- Before finishing, stop temporary processes you started. Leave maintainer-owned processes running.

## Code style preferences
- Prefer compact, elegant code that keeps the local control flow easy to read. Extract helpers when they name a real concept, isolate complex behavior, or remove meaningful duplication.
- Avoid one-line helper functions used in a single place; they usually make this codebase harder to read than an inline expression.
- Backend and worker code run as TypeScript on Node 24 with `erasableSyntaxOnly`; keep local imports explicit with `.ts` extensions.

## High-level structure
- `backend/`: Express + PostGraphile 5 (Amber preset) server. Custom plugins live in `backend/src/plugins` (S3-backed file URLs, current-user fields, and person membership filters). `backend/src/auth.ts` resolves the request tenant, verifies JWTs, and maps their claims to database settings.
- `frontend/`: Next.js 16 App Router app using TypeScript, Tailwind, and URQL. Shared UI primitives are in `frontend/ui`, routes in `frontend/app`, feature-specific modules in folders such as `frontend/calendar`, `frontend/scoreboard`, and `frontend/lib`. Tenant-specific overrides live in `frontend/tenant`.
- `worker/`: Graphile Worker package. Queue tasks live in `worker/tasks`, MJML email templates in `worker/templates`, and the federated dance-data crawler/frontier system in `worker/crawler`.
- `graphql/`: Source `.graphql` operation documents consumed by GraphQL Code Generator. The generated TypeScript bindings land near their usage in `frontend/graphql`.
- `e2e/`: Playwright smoke tests and auth fixtures.
- `migrations/`: Graphile Migrate directory layout. `committed/` holds historical migrations, `current/1-current.sql` is the scratchpad for new idempotent changes, `fixtures/` contains repeatable helper SQL/PLpgSQL objects, and `initial_schema.sql` mirrors the baseline dump.
- `schema.sql`: pg_dump of the live database, including extensions and RLS policies. Use it together with `schema/split.py` to keep the `schema/` tree synchronized.
- `schema/`: Auto-split DDL organized by domain/type/table/function/view for review purposes only.

## Frontend tenancy model
- Tenant host mapping lives in `frontend/tenant/catalog.ts`. `frontend/proxy.ts` preserves a valid `tenant_id` cookie; otherwise it selects the host's tenant or the default tenant.
- Tenant-specific assets/config live under `frontend/tenant/{olymp,kometa,starlet}`. `frontend/tenant/ui.pages.ts` wires those configs to dynamically loaded tenant UI components.
- Shared tenant metadata/types sit in `frontend/tenant/types.ts`; use these helpers when adding new tenant-aware UI.
- Pages and components should read the active tenant configuration rather than hard-coding IDs. `frontend/lib/query.ts` injects the active `x-tenant-id` header for URQL requests.

## Backend tenancy model
- `backend/src/auth.ts` determines the tenant for each request. It:
  - inspects `x-tenant-id` headers or matches `req.hostname`/`origin` against `tenant.origins` in the database,
  - defaults to tenant `1` when nothing matches,
  - builds `pgSettings` so Postgres row-level security sees `jwt.claims.tenant_id`, memberships, and role claims.
- `backend/src/graphile.config.ts` forwards request `pgSettings` into Grafast/PostGraphile and configures the PostGraphile schema.
- `current_tenant_id()` (defined in `migrations/committed/000026.sql`) returns the active tenant ID from the current PostgreSQL session (defaulting to `1`). Nearly every table includes a `tenant_id` column defaulting to this function.
- `app_private.create_jwt_token(users)`, maintained in `migrations/fixtures/functions/create_jwt_token.sql`, builds claims directly from `user_proxy`, active tenant/cohort memberships, active couples, and system-admin status. The old `auth_details` views and refresh jobs have been removed.
- `loadUserFromSession` accepts a bearer token or the `rozpisovnik` cookie. It chooses the database role from the token's system-admin flag and tenant-specific role arrays, then copies claims into `jwt.claims.*` settings. The request tenant overrides the token's `tenant_id`; memberships are not reloaded on each request. Bearer token use on the frontend is deprecated, stays available for potential external consumers.
- Both backend verification and `frontend/lib/server/tenant.ts` currently use `ignoreExpiration: true`. JWTs contain a seven-day expiry, but verification does not enforce it - intentionally postponed, will be relevant soon.
- Event-share access is resolved separately through `app_private.event_share_claims`, which populates `jwt.claims.shared.*` settings for the request.
- When adding claims, update the `jwt_token` composite type, `create_jwt_token`, frontend claim types, and any required anonymous defaults. Check both backend and frontend database-context builders; both map claim arrays to PostgreSQL array literals.

## Session refresh and RLS patterns
- Web login and refresh actions in `frontend/lib/auth-actions.ts` set an HttpOnly session cookie. `current_claims()` and `refresh_jwt()` rebuild claims from the database; `SessionRefresher` checks for changes every 30 seconds while the page is visible. Legacy browser-token migration remains in place; see `TODO-auth.md`.
- For example, `current_person_ids()` and related helpers read session claims. `app_private.visible_person_ids()` instead derives a set of visible people from current-tenant membership views; person and couple policies use uncorrelated `IN (SELECT ...)` lookups against that set. Its fixture is `migrations/fixtures/functions/visible_person_ids.sql`.

## Database entities worth knowing
- `tenant`: master tenant records with allowed `origins` for host matching.
- `users` + `user_proxy`: authentication identities linked to `person` records; JWT creation relies on `app_private.create_jwt_token`.
- `person`, `couple`, `cohort`, `event_*`, `payment_*`: core CRM/scheduling/accounting tables, each row-secured by tenant.
- `tenant_settings`: key/value settings scoped by tenant.
- Supporting functions (examples under `schema/functions/public.*.sql`) expose helper queries like `filtered_people`, `create_missing_cohort_subscription_payments`, and `get_current_tenant()`; many assume `current_tenant_id()` is set.

## GraphQL stack
- PostGraphile auto-exposes the PostgreSQL schema with additional fields and filters from custom plugins (S3 file URLs, current-user helpers, and person membership filters).
- The frontend consumes the API via URQL. Operation documents reside in `graphql/*.graphql`, and typed documents live alongside feature code in `frontend/graphql`. Avoid hand-editing generated files!
- `graphql.config.yml` and `graphql-starlet.config.yml` configure code generation for different tenant bundles.

## Worker and crawler model
- Graphile Worker loads TypeScript tasks through `worker/graphile.config.ts`. `worker/crontab` schedules membership refreshes, accounting/event discovery, and `frontier_schedule`; the scheduler seeds root frontiers and enqueues `frontier_fetch`/`frontier_process` jobs as needed.
- The crawler stores work in `crawler.frontier`. Loader definitions in `worker/crawler/handlers.ts` fetch federation-specific JSON or HTML, persist raw responses, and then normalize them through each loader's `load` handler into federated tables. Loader side effects are batched through `worker/crawler/effects.ts`.
- JSON loaders define Zod schemas. After changes to schemas or loaders, run `pnpm --silent crawler backtest <federation>:<kind>` against cached responses.
- Local crawler development uses the root CLI in `worker/crawler/cli.ts`: run `pnpm crawler ...` with commands like `list`, `status [federation]:[kind]`, `failures ...`, `jobs ...`, `explain ...`, `response ...`, `refetch ...`, `cleanup ... [--commit]`, `backtest ...`, and `process ... [--commit]`.
- Fetch responses are stored in `crawler.json_response` / `crawler.json_response_cache`; each frontier points to its latest response and latest successful response so operational reads do not scan history.
- Crawler SQL lives in `worker/crawler/*.sql`; pgtyped outputs `worker/crawler/*.queries.ts`. When SQL changes, regenerate the typed queries with `pnpm --filter @rozpisovnik/worker sql:generate` instead of hand-editing the generated files.
- Prefer bulk loader queries shaped as `pgtypedCollection` + `unnest` arrays. Keep per-loader query count low, and make merge/upsert statements semantic no-ops on repeated loads except where the table is intentionally cleared and reinserted.
- Normalize incoming federation quirks in Zod schemas or enum mappers before load logic. Keep loader bodies focused on building federated rows and frontier keys.
- Use the crawler dev tool for cached inspection and replay: `pnpm --silent crawler response <frontier-key> | jq ...` for response bodies, and `pnpm crawler process <frontier-key>` for rollback-by-default validation. Use `--commit` only when intentionally replaying into the dev database.

## Frontend conventions
- This is an App Router app. Use the `@/*` import alias to reference files from the frontend root.
- We use Radix primitives wrapped in our custom wrappers.
- We use Tailwind processed Radix colors. In the project they are aliased as `accent` and `neutral`, with the usual scale 1 to 12 (`bg-neutral-2`, `text-accent-11`). We don't use shadcn colors (border, background, etc.).

- 1: App background (page, root, outer container) → Use for body, app shell, or scroll areas.
- 2: Slightly raised elements (cards, panels, sections) → Use for cards, secondary surfaces, or hover backgrounds on dark text.
- 3: Input backgrounds, subtle separators → Use for form fields, menu items, table rows, nested surfaces.
- 4: Neutral border, subtle component outlines. → Use for borders, dividers, disabled controls, tooltips.
- 5: Interactive hover states (backgrounds that respond). → Use for button hover, list hover, tabs, switch track.
- 6: More prominent but still restrained backgrounds. → Use for active background, selected state, focused field bg.
- 7: Default solid surfaces or filled components. → Use for filled buttons, accent UI, highlighted areas.
- 8: Hover/active state for filled UI, or strong accent background. → Use for button hover, selected tab bg, slider fill, badge bg.
- 9: Base accent color (the “brand” shade). → Use for primary buttons, links, toggles, charts, icons.
- 10: Text/foreground on colored backgrounds. → Use for text over accent (on 9/8), icons, badges, contrast overlays.
- 11: Strong text color within accent schemes. → Use for headings, highlighted text, active icon, focus border.
- 12: Primary foreground (text/icons on neutral backgrounds). → Use for body text, titles, critical info, any high-contrast content.
- Avoid using 9–12 for large surfaces; keep them for accents or text.
- Step down (e.g., 3–5) for backgrounds, up (10–12) for text/foreground.
- For dark themes, Radix automatically inverts the perceptual weight—keep the same numeric semantics.

## Common tasks & commands
- Run all typechecks: `pnpm -r typecheck`
- Run all lints: `pnpm -r lint`
- Type-check/lint the API: `pnpm --filter @rozpisovnik/backend lint`, `pnpm --filter @rozpisovnik/backend typecheck`.
- Frontend checks use `pnpm --filter @rozpisovnik/web lint`, `pnpm --filter @rozpisovnik/web typecheck`; the Next build is slow.
- Run queue workers: `pnpm --filter @rozpisovnik/worker start`
- Run the crawler dev tool: `pnpm crawler --help`
- Type-check/lint the worker: `pnpm --filter @rozpisovnik/worker lint`, `pnpm --filter @rozpisovnik/worker typecheck`
- Run Playwright smoke tests: `pnpm --filter @rozpisovnik/e2e test` (defaults to `PLAYWRIGHT_BASE_URL=http://localhost:5100`).
- Create a new migration: follow `migrations/current/AGENTS.md`, including its fixture inclusion and testing steps.
- Add GraphQL documents to the root `graphql/` folder. Regenerate bindings with `pnpm schema` or `pnpm schema-starlet`.
- After frontend SQL changes, regenerate query bindings with `pnpm --filter @rozpisovnik/web sql:generate`.
- Leave regenerated files in the working tree for the maintainer to review.
- Do not hand-patch or revert generated output to restore its previous state or reduce the diff. This includes `frontend/graphql/`, `schema.sql`, `schema/`, `schema.graphql`, and `*.queries.ts`. If output is wrong, fix the source or generator and regenerate it. Follow explicit user instructions for any requested rollback.

Keep this guide in sync as the project evolves.
