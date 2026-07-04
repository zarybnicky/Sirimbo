# syntax=docker/dockerfile:1.7

FROM node:24-bookworm-slim AS base

WORKDIR /repo

ENV PNPM_HOME=/pnpm
ENV PATH=/pnpm:$PATH
ENV NEXT_TELEMETRY_DISABLED=1

RUN corepack enable && pnpm config set store-dir /pnpm/store

FROM base AS deps

COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches/ patches/
COPY frontend/package.json frontend/package.json

RUN --mount=type=cache,id=pnpm-web,target=/pnpm/store,sharing=locked \
  pnpm install --frozen-lockfile --filter @rozpisovnik/web...

FROM deps AS builder

ARG NEXT_PUBLIC_GRAPHQL_BACKEND
ARG NEXT_PUBLIC_SENTRY_DSN
ARG NEXT_PUBLIC_SENTRY_ENVIRONMENT
ARG SENTRY_AUTH_TOKEN
ARG SENTRY_RELEASE
ARG GITHUB_SHA

COPY . .

RUN --mount=type=cache,id=next-web,target=/repo/frontend/.next/cache,sharing=locked \
  pnpm --filter @rozpisovnik/web build

FROM gcr.io/distroless/nodejs24-debian12:nonroot AS runner

WORKDIR /app

ENV NODE_ENV=production
ENV NEXT_TELEMETRY_DISABLED=1
ENV HOSTNAME=0.0.0.0
ENV PORT=3000
ENV NEXT_SHARP_PATH=/app/frontend/node_modules/sharp

COPY --from=builder --chown=65532:65532 /repo/frontend/.next/standalone ./
COPY --from=builder --chown=65532:65532 /repo/frontend/.next/static ./frontend/.next/static
COPY --from=builder --chown=65532:65532 /repo/frontend/public ./frontend/public

EXPOSE 3000

CMD ["frontend/server.js"]
