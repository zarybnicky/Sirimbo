# Sirimbo repository map and contributor notes

This document is for fellow ChatGPT/Codex-style agents working in this repository. Use it as a quick reference before you make changes.

## Workflow expectations
- Use the checked-in PNPM 9 workspaces (Node 22.x) for JavaScript/TypeScript tooling.
- Keep the repo tidy: run targeted commands rather than regenerating every artifact. In particular **do not commit regenerated GraphQL codegen output** (the typed client definitions in `frontend/graphql` and `graphql/`); only update them when explicitly requested and include them in a separate PR if needed.
- Prefer incremental SQL migrations. When altering the database, author idempotent scripts in `migrations/current/1-current.sql` (or add fixtures under `migrations/fixtures/...`) and rely on Graphile Migrate to promote them.
- `schema/` is generated from the canonical `schema.sql` dump via `python schema/split.py < schema.sql`. Do not hand-edit files under `schema/`—regenerate from the dump instead.
- When making frontend changes, ensure `pnpm --filter @rozpisovnik/web build` completes successfully and report the command under Testing.

## High-level structure
- `backend/`: Express + PostGraphile 5 (Amber preset) server. Custom plugins live in `backend/src/plugins` (S3-backed file URLs and HTTP proxy resolvers). Multi-tenancy and JWT enrichment are handled in `backend/src/graphile.config.ts`.
- `frontend/`: Next.js 15 app using TypeScript, Tailwind, and URQL. Shared UI primitives are in `frontend/ui`, pages in `frontend/pages`, feature-specific modules in folders such as `frontend/calendar`, `frontend/components`, and `frontend/lib`. Tenant-specific overrides live in `frontend/tenant`.
- `worker/`: Graphile Worker tasks (email, invitations, auth refresh) with templates in `worker/templates` and task handlers in `worker/tasks`.
- `graphql/`: Source `.graphql` operation documents consumed by GraphQL Code Generator. The generated TypeScript bindings land near their usage in `frontend/graphql` (again: avoid committing regenerated outputs unless the task requires it).
- `migrations/`: Graphile Migrate directory layout. `committed/` holds historical migrations, `current/1-current.sql` is the scratchpad for new idempotent changes, `fixtures/` contains repeatable helper SQL/PLpgSQL objects, and `initial_schema.sql` mirrors the baseline dump.
- `schema.sql`: pg_dump of the live database, including extensions and RLS policies. Use it together with `schema/split.py` to keep the `schema/` tree synchronized.
- `schema/`: Auto-split DDL organized by domain/type/table/function/view for review purposes only.

## Frontend tenancy model
- Multi-tenant builds use `frontend/tenant/config.js` to select the runtime tenant profile based on `NEXT_PUBLIC_TENANT_ID`. Tenant-specific assets/config live under `frontend/tenant/{olymp,kometa,starlet}`.
- Shared tenant metadata/types sit in `frontend/tenant/types.ts`; use these helpers when adding new tenant-aware UI.
- Pages and components should read the active tenant configuration rather than hard-coding IDs. Check `frontend/lib` for data loaders and URQL exchanges that inject tenant-aware headers.

## Backend tenancy model
- `backend/src/graphile.config.ts` determines the tenant for each request. It:
  - inspects `x-tenant-id` headers or matches `req.headers.host`/`origin` against `tenant.origins` in the database,
  - defaults to tenant `1` when nothing matches,
  - builds `pgSettings` so Postgres row-level security sees `jwt.claims.tenant_id`, memberships, and role claims.
- `current_tenant_id()` (defined in `migrations/committed/000026.sql`) returns the active tenant ID from the current PostgreSQL session (defaulting to `1`). Nearly every table includes a `tenant_id` column defaulting to this function.
- Membership tables (`tenant_membership`, `tenant_trainer`, `tenant_administrator`) along with `tenant_settings` and RLS policies guard per-tenant access. The `app_private.auth_details` view aggregates per-person memberships for JWT enrichment.
- When extending tenancy logic, update the database policies first, then ensure `loadUserFromSession` populates the corresponding JWT claims.

## Database entities worth knowing
- `tenant`: master tenant records with allowed `origins` for host matching.
- `users` + `user_proxy`: authentication identities linked to `person` records; JWT creation relies on `app_private.create_jwt_token`.
- `person`, `couple`, `cohort`, `event_*`, `payment_*`: core CRM/scheduling/accounting tables, each row-secured by tenant.
- `tenant_settings`: key/value settings scoped by tenant.
- Supporting functions (examples under `schema/functions/public/`) expose helper queries like `filtered_people`, `create_missing_cohort_subscription_payments`, and `get_current_tenant()`; many assume `current_tenant_id()` is set.

## GraphQL stack
- PostGraphile auto-exposes the PostgreSQL schema with additional fields from custom plugins (S3 file URLs, HTTP proxying).
- The frontend consumes the API via URQL. Operation documents reside in `graphql/*.graphql`, and typed documents live alongside feature code in `frontend/graphql`. Keep documents and generated types aligned, but avoid committing regenerated artifacts unless asked.
- `graphql.config.yml` and `graphql-starlet.config.yml` configure code generation for different tenant bundles.

## Frontend conventions
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
- Run the API in development: `pnpm --filter rozpisovnik-api start` (expects `DATABASE_URL`, `JWT_SECRET`, S3 env vars, etc.).
- For quick development work, use primarily `tsc` to check correctness - the Next build is slow
- Build and lint the Next.js app after changes: `pnpm --filter @rozpisovnik/web lint` / `build`.
- Run queue workers: `pnpm --filter @rozpisovnik/worker start`
- Update the split schema after refreshing `schema.sql`: `python schema/split.py < schema.sql`.
- Create a new migration: write to `migrations/current/`, named like `1-event-attendance.sql`, prefixed by an increasing number, in kebab-case. Ensure scripts are idempotent.
- Don't add GraphQL documents to code, add them to the root graphql/ folder, and run graphql-codegen, which will generate types and documents in `frontend/graphql/`.

Keep this guide in sync as the project evolves.
