# syntax=docker/dockerfile:1

FROM node:24-bookworm-slim AS base

WORKDIR /repo

ENV PNPM_HOME=/pnpm
ENV PATH=/pnpm:$PATH
ENV NEXT_TELEMETRY_DISABLED=1

RUN corepack enable && pnpm config set store-dir /pnpm/store

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*

FROM base AS deps

COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches/ patches/
COPY frontend/package.json frontend/package.json

RUN --mount=type=cache,id=sirimbo-pnpm,target=/pnpm/store,sharing=locked \
  pnpm install --frozen-lockfile --filter rozpisovnik --filter @rozpisovnik/web...

FROM deps AS graphql-validation

COPY graphql.config.yml ./
COPY graphql/ graphql/
ARG NEXT_PUBLIC_GRAPHQL_BACKEND
RUN test -n "$NEXT_PUBLIC_GRAPHQL_BACKEND" \
  && GRAPHQL_SCHEMA="${NEXT_PUBLIC_GRAPHQL_BACKEND%/}/graphql" pnpm schema

FROM deps AS builder

COPY frontend/ frontend/
COPY --from=graphql-validation /repo/frontend/graphql /repo/frontend/graphql

ARG NEXT_PUBLIC_GRAPHQL_BACKEND
ARG NEXT_PUBLIC_SENTRY_DSN
ARG NEXT_PUBLIC_SENTRY_ENVIRONMENT
ARG SENTRY_AUTH_TOKEN

# Build metadata must not invalidate the cache for unchanged application inputs.
RUN --mount=type=cache,id=sirimbo-next,target=/repo/frontend/.next/cache,sharing=locked \
    --mount=type=secret,id=GITHUB_SHA,env=GITHUB_SHA,required=true \
  NEXT_DEPLOYMENT_ID="$GITHUB_SHA" SENTRY_RELEASE="$GITHUB_SHA" pnpm --filter @rozpisovnik/web build

FROM gcr.io/distroless/nodejs24-debian12:nonroot AS runner

WORKDIR /app

ENV NEXT_SHARP_PATH=/app/frontend/node_modules/sharp
ENV NEXT_TELEMETRY_DISABLED=1
ENV NODE_ENV=production
ENV HOSTNAME=0.0.0.0
ENV PORT=3000
EXPOSE 3000

COPY --from=builder --chown=65532:65532 /repo/frontend/.next/standalone ./
COPY --from=builder --chown=65532:65532 /repo/frontend/.next/static ./frontend/.next/static
COPY --from=builder --chown=65532:65532 /repo/frontend/public ./frontend/public

CMD ["frontend/server.js"]
