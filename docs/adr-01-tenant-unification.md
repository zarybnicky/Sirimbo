# ADR 01: Unified tenant deployment

- **Status**: Proposed
- **Date**: 2024-05-18
- **Decision drivers**: Consolidate per-tenant builds into a single deployment while preserving `X-Tenant-ID` enforcement and allowing runtime tenant switching.

## Context

Production currently ships one Next.js build per tenant by hard-coding `NEXT_PUBLIC_TENANT_ID`, with URQL always attaching the same `X-Tenant-ID` header.【F:frontend/tenant/config.js†L1-L27】【F:frontend/graphql/query.ts†L32-L34】【F:frontend/graphql/query.ts†L149-L166】 The backend resolves the tenant in `findTenantId` by trusting the header first and only falling back to host matching, which means traffic that arrives via the shared `rozpisovnik.cz` host without a header is forced onto tenant `1`.【F:backend/src/graphile.config.ts†L17-L31】

The split deployment model causes duplicated CI/CD pipelines, makes tenant switching painful, and prevents the shared root domain from serving multiple tenants without custom proxies.

## Decision

We will serve every tenant from a single Next.js + PostGraphile deployment and derive the active tenant at runtime using a hard-coded catalogue until the backend exposes a live feed.

### Runtime tenant catalogue

Create `frontend/tenant/catalog.ts` that exports a hard-coded `tenancyCatalog` array plus lookup maps keyed by tenant ID and hostname. Each entry must include:

- Numeric tenant ID and slug used across the stack.
- Display metadata (name, theme colors, logos) that mirror what is currently embedded in `frontend/tenant/{tenant}/config` modules.【F:frontend/tenant/config.js†L4-L27】【F:frontend/components/layout/Footer.tsx†L83-L86】
- Lazy-load component overrides (hero banners, layout slices) via dynamic imports so the unified bundle only fetches tenant-specific UI when needed (see "Lazy tenant components" below).
- Allowed hostnames, covering both `<tenantSlug>.rozpisovnik.cz` and `rozpisovnik.cz` for tenants that should appear on the shared root.

The module will remain in the repo until the backend surfaces an equivalent query; its API should match the future GraphQL response so swapping the data source requires no downstream changes.

### Tenant resolution order

The application must stay cookie-free for tenancy. The active tenant is resolved in this precedence:

1. **Hostname match** – if the request host matches one of the catalogue entries (e.g. `kometa.rozpisovnik.cz`), the corresponding tenant is selected automatically, ensuring backwards compatibility for slugged deployments.
2. **JWT fallback** – when no host match exists (e.g. `rozpisovnik.cz`), choose the first tenant listed in the authenticated user's unified JWT. `frontend/ui/state/auth.ts` will decode `{ tenants: { [tenantId]: { roles... } } }` so the provider can default safely.【F:frontend/ui/state/auth.ts†L56-L86】
3. **Client persistence (optional)** – after the initial selection, the provider may store the tenant ID in `localStorage` to reuse on client-side transitions, but SSR must always derive the first value from hostname or JWT to avoid stateful cookies.

The frontend must continue sending the resolved tenant through the `X-Tenant-ID` header on every GraphQL request so the backend can keep enforcing row-level security without additional session state.

### Frontend architecture

- Introduce a `TenantProvider` React context that:
  - Reads the forwarded hostname during SSR (via middleware-provided headers) and applies the resolution order above before first render.
  - Hydrates tenant metadata from `tenancyCatalog` without additional network calls.
  - Exposes `{ tenantId, tenant, setTenantId }`, derived theme helpers, lazy component factories (see below), and a callback (`resetUrqlClient`) to flush caches when the tenant changes.
  - Invokes `storeRef.resetUrqlClient?.()` whenever `setTenantId` runs so URQL reinitializes exchanges under the new tenant and refreshes `CurrentUser`.
- Wrap `_app.tsx` with `TenantProvider` and update all modules that currently read `NEXT_PUBLIC_TENANT_ID` (GraphQL clients, footer overrides, feature pages) to consume the provider or shared selector functions instead.【F:frontend/graphql/query.ts†L25-L74】【F:frontend/pages/starlet-import/index.tsx†L14-L16】【F:frontend/components/layout/Footer.tsx†L83-L86】
- Build a dropdown switcher in the global layout that lists every catalogue entry. On `rozpisovnik.cz` it updates the provider state; on slugged hosts it navigates to the selected tenant's canonical hostname. In both cases it clears URQL caches to avoid cross-tenant data leakage.
- Extend `frontend/ui/state/auth.ts` so role flags are keyed by tenant ID, expose selectors that accept an explicit tenant (e.g. `useTenantMembership(tenantId)` for cross-tenant dashboards), and recompute derived flags whenever the active tenant changes.
- Ship a unified JWT payload that mirrors the runtime catalogue. Instead of the current top-level `is_member`/`is_trainer`/`is_admin` booleans, the token should embed per-tenant envelopes:

Because the initial catalogue is hard-coded, the provider, dropdown, and auth changes can ship (and be tested) ahead of the backend GraphQL field; swapping the data source later becomes a drop-in replacement inside `tenant/catalog.ts`.

  ```json
  {
    "sub": "<user-id>",
    "default_tenant_id": 1,
    "tenants": {
      "1": {
        "slug": "kometa",
        "roles": { "isMember": true, "isTrainer": false, "isAdmin": false },
        "personIds": ["person-1"],
        "featureFlags": ["payments"],
        "primaryPersonId": "person-1"
      },
      "2": {
        "slug": "olymp",
        "roles": { "isMember": true, "isTrainer": true, "isAdmin": true },
        "personIds": ["person-2", "person-3"],
        "featureFlags": ["invoices", "trainings"],
        "primaryPersonId": "person-2"
      }
    },
    "global": {
      "email": "user@example.com",
      "uLogin": "user",
      "membershipIds": ["membership-1", "membership-2"]
    }
  }
  ```

  This layout keeps the JWT backwards compatible with the existing auth module by preserving role flags while giving the frontend enough context to select a deterministic fallback tenant, hydrate dropdown labels, and guard SSR against host mismatches.【F:frontend/ui/state/auth.ts†L56-L86】
- Ensure `UserRefresher` (and other URQL hooks tied to tenant-specific data) include the active tenant in their query keys or trigger refetches after a switch so memberships and permissions rehydrate under the new context.【F:frontend/ui/use-auth.tsx†L8-L34】

### Middleware and SSR support

Add a minimal `frontend/middleware.ts` that forwards the canonical host into a header the provider and URQL can read during SSR. Create a companion `withTenantHost` helper for API routes so internal fetch handlers pick up the same header contract:

```ts
import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

export const config = { matcher: ["/:path*"] };

export function middleware(request: NextRequest) {
  const response = NextResponse.next();
  const host = request.headers.get("x-forwarded-host") ?? request.nextUrl.host;

  if (host) {
    response.headers.set("x-tenant-host", host.toLowerCase());
  }

  return response;
}
```

During SSR, the provider reads `x-tenant-host` via `headers()`; on the client, the URQL exchange forwards it alongside the computed `X-Tenant-ID` header so server-side GraphQL calls resolve the same tenant that the browser sees. Update `frontend/graphql/query.ts` (and any bespoke fetch helpers) so `fetchOptions` reads the active tenant from the provider, always emits both headers, and re-keys or flushes Graphcache when the tenant changes to avoid leaking cached responses across users.【F:frontend/graphql/query.ts†L25-L74】【F:frontend/graphql/query.ts†L149-L166】

### Backend adjustments

- Keep `findTenantId` trusting `X-Tenant-ID` while still supporting host-based fallbacks for `<tenantSlug>.rozpisovnik.cz` traffic, ensuring legacy bookmarks continue to function.【F:backend/src/graphile.config.ts†L17-L31】 Continue enriching JWTs so `jwt.claims.tenant_id` reflects the resolved tenant and Postgres RLS never sees a stale value.
- Add a GraphQL `tenancyCatalog` field backed by a view that exposes safe tenant metadata. Once available, the frontend can replace the hard-coded module with this query without refactoring call sites.
- Ship a parallel, code-side `tenantComponentRegistry` keyed by tenant slug so lazy-loaded component overrides remain tree-shakeable even after metadata starts coming from the database (see "Lazy tenant components").
- Seed `tenant_settings` with the current theme JSON so the backend becomes the long-term source of truth.

## Consequences

### Positive

- A single CI/CD pipeline delivers the frontend for every tenant, simplifying deployments.
- Support and admins can switch tenants quickly without juggling multiple browser tabs.
- Backend RLS policies remain unchanged because `X-Tenant-ID` continues to disambiguate requests.

### Negative / Risks

- URQL cache leaks must be mitigated by flushing or namespacing caches when the tenant changes.
- Theme flashes can occur if SSR fails to preload tenant metadata; ensure middleware and provider logic are in place before rollout.
- URL tampering is possible on the shared host; validate that the requested tenant exists in the user's JWT memberships before issuing elevated permissions or rendering sensitive data.

## Follow-up actions

1. Populate `tenant_settings` with current theme JSON via an idempotent migration.
2. Implement the GraphQL catalogue view/resolver.
3. Build the `TenantProvider`, dropdown switcher, and URQL wiring while keeping the existing `NEXT_PUBLIC_TENANT_ID` fallback for legacy builds; remove the flag once the unified deployment is live.
4. Document operator workflows for adding new tenants via database updates instead of bespoke deployments.

## Appendix

### Example `tenancyCatalog`

```ts
import type { ComponentType } from "react";

export type TenancyCatalogEntry = {
  id: number;
  slug: string;
  name: string;
  hosts: string[]; // e.g. ["kometa.rozpisovnik.cz", "rozpisovnik.cz"]
  theme: {
    primary: string;
    secondary: string;
  };
  logo: string;
  components: {
    hero?: () => Promise<{ default: ComponentType }>; // React.lazy compatible
    footer?: () => Promise<{ default: ComponentType }>;
  };
};

export const tenancyCatalog: TenancyCatalogEntry[] = [
  {
    id: 1,
    slug: "kometa",
    name: "Kometa",
    hosts: ["kometa.rozpisovnik.cz", "rozpisovnik.cz"],
    theme: {
      primary: "#005eb8",
      secondary: "#ffffff",
    },
    logo: "/images/tenants/kometa/logo.svg",
    components: {
      hero: () => import("../tenant/kometa/Hero"),
      footer: () => import("../tenant/kometa/FooterOverride"),
    },
  },
  {
    id: 2,
    slug: "olymp",
    name: "Olymp",
    hosts: ["olymp.rozpisovnik.cz", "rozpisovnik.cz"],
    theme: {
      primary: "#ff6f00",
      secondary: "#2a2a2a",
    },
    logo: "/images/tenants/olymp/logo.svg",
    components: {
      hero: () => import("../tenant/olymp/Hero"),
    },
  },
];

export const tenancyByHost = new Map(
  tenancyCatalog.flatMap((tenant) => tenant.hosts.map((host) => [host, tenant]))
);
```

### Lazy tenant components

- Expose lazy component loaders on each catalogue entry instead of importing the tenant-specific modules at the top level. `TenantProvider` can wrap them with `React.lazy`/`next/dynamic` so overrides download only when rendered, keeping the shared bundle lean.
- When the catalogue eventually moves to the database, keep the dynamic import table (`tenantComponentRegistry`) in the frontend keyed by slug and merge it with the fetched metadata (e.g. `const runtimeCatalog = dbCatalog.map((tenant) => ({ ...tenant, components: componentRegistry[tenant.slug] ?? {} }))`). This preserves lazy loading without serializing functions in the database.
- Document the registry shape so adding a new tenant in the database requires a matching entry in the component registry (and a CI lint check or runtime assertion can enforce the mapping before overrides go missing).

### Minimal `frontend/middleware.ts`

See the implementation in the "Middleware and SSR support" section above for the code that forwards `x-tenant-host` to SSR/CSR callers.

### Testing guidance

- Add React Testing Library coverage for the `TenantProvider` and dropdown to verify hostname-derived defaults, JWT fallbacks, cache resets, and URQL header wiring as the catalogue evolves.
