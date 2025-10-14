# Standalone Docker images

The repository now ships multi-stage Dockerfiles for the three runtime services. Each build relies solely on the Yarn 4 workspaces and the standard Next.js build pipeline—no Nix tooling or flakes are required. All container definitions and documentation live in this `docker/` directory for easy discovery.

## Backend API (`docker/api.Dockerfile`)

```
docker build -f docker/api.Dockerfile -t rozpisovnik-api .
```

The builder stage focuses the `rozpisovnik-api` workspace, bundles `src/index.ts` with `esbuild`, and prunes dependencies to production-only packages. The runtime stage starts the compiled PostGraphile server on port 5000 using Node 22.

## Graphile Worker (`docker/worker.Dockerfile`)

```
docker build -f docker/worker.Dockerfile -t rozpisovnik-worker .
```

The Dockerfile compiles the TypeScript task handlers alongside `graphile.config.ts` and ships them with the Graphile Worker CLI. The container launches `graphile-worker --config worker/graphile.config.js`, so set `DATABASE_URL` and SMTP-related environment variables when running the image.

## Next.js frontend (`docker/frontend.Dockerfile`)

```
docker build -f docker/frontend.Dockerfile -t rozpisovnik-web .
```

This image runs `yarn workspace rozpisovnik-web build` to produce the standard Next.js standalone output, copies `.next/standalone`, static assets, and `public/` into a slim Node 22 runtime, and listens on port 3000. Supply the usual `NEXT_PUBLIC_*` and backend endpoint variables at runtime.
