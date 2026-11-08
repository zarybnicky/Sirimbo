# syntax=docker/dockerfile:1

FROM node:24-bookworm-slim AS deps

WORKDIR /repo

ENV PNPM_HOME=/pnpm
ENV PATH=/pnpm:$PATH

RUN corepack enable && pnpm config set store-dir /pnpm/store

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches/ patches/
COPY worker/package.json worker/package.json

RUN --mount=type=cache,id=sirimbo-pnpm,target=/pnpm/store,sharing=locked \
  pnpm install --frozen-lockfile --filter @rozpisovnik/worker...

COPY worker/ worker/

RUN pnpm --filter @rozpisovnik/worker typecheck \
    && CI=true pnpm -C worker prune --prod

FROM gcr.io/distroless/nodejs24-debian12:nonroot

WORKDIR /app/worker

ENV NODE_ENV=production
ENV RUIAN_ADDRESS_SOURCE=/app/data/ruian.duckdb
ENV GEONAMES_CITY_SOURCE=/app/data/geonames.duckdb
ENV NODE_OPTIONS="--require /app/worker/node_modules/@hyperdx/node-opentelemetry/build/src/tracing.js"
ENV OTEL_SERVICE_NAME=rozpisovnik-worker
# Let Graphile Worker finish jobs and release locks on SIGTERM.
ENV HDX_NODE_STOP_ON_TERMINATION_SIGNALS=false
ENV HDX_NODE_EXPERIMENTAL_EXCEPTION_CAPTURE=1
ENV HDX_NODE_BETA_MODE=1

COPY --from=deps /repo/node_modules /app/node_modules
COPY --from=deps /repo/worker /app/worker
# Supplied by --build-context worker-data=... after building the Nix datasets.
COPY --from=worker-data / /app/data/

CMD ["rozpisovnik-worker", "-j", "5"]
