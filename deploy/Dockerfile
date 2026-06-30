# syntax=docker/dockerfile:1.7

# Run as:
# docker buildx build \
#   --file deploy/Dockerfile \
#   --target runner \
#   --platform linux/amd64 \
#   --build-arg NEXT_PUBLIC_GRAPHQL_BACKEND=https://api.rozpisovnik.cz \
#   --build-arg NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA=${GIT_SHA} \
#   --cache-from type=registry,ref=registry.local:5000/rozpisovnik/web:buildcache-main \
#   --cache-to type=registry,ref=registry.local:5000/rozpisovnik/web:buildcache-main,mode=max \
#   --tag registry.local:5000/rozpisovnik/web:${GIT_SHA} \
#   --push \
#   .

ARG NODE_IMAGE=node:24-bookworm-slim
ARG RUNTIME_IMAGE=gcr.io/distroless/nodejs24-debian12:nonroot

FROM ${NODE_IMAGE} AS base

WORKDIR /repo

ENV PNPM_HOME=/pnpm
ENV PATH=/pnpm:$PATH
ENV NEXT_TELEMETRY_DISABLED=1

RUN corepack enable \
  && pnpm config set store-dir /pnpm/store

FROM base AS deps

ARG TARGETOS
ARG TARGETARCH
ARG TARGETVARIANT

COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches/ patches/

COPY frontend/package.json frontend/package.json

RUN --mount=type=cache,id=pnpm-web-${TARGETOS}-${TARGETARCH}-${TARGETVARIANT},target=/pnpm/store,sharing=locked \
  pnpm install --frozen-lockfile --filter @rozpisovnik/web...

FROM deps AS builder

ARG NEXT_PUBLIC_GRAPHQL_BACKEND
ARG NEXT_PUBLIC_SENTRY_DSN
ARG NEXT_PUBLIC_SENTRY_ENVIRONMENT
ARG NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA

COPY . .

RUN --mount=type=cache,id=next-web-${TARGETOS}-${TARGETARCH}-${TARGETVARIANT},target=/repo/frontend/.next/cache,sharing=locked \
  env \
    -u NEXT_PUBLIC_GRAPHQL_BACKEND \
    -u NEXT_PUBLIC_SENTRY_DSN \
    -u NEXT_PUBLIC_SENTRY_ENVIRONMENT \
    -u NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA \
    ${NEXT_PUBLIC_GRAPHQL_BACKEND:+NEXT_PUBLIC_GRAPHQL_BACKEND=$NEXT_PUBLIC_GRAPHQL_BACKEND} \
    ${NEXT_PUBLIC_SENTRY_DSN:+NEXT_PUBLIC_SENTRY_DSN=$NEXT_PUBLIC_SENTRY_DSN} \
    ${NEXT_PUBLIC_SENTRY_ENVIRONMENT:+NEXT_PUBLIC_SENTRY_ENVIRONMENT=$NEXT_PUBLIC_SENTRY_ENVIRONMENT} \
    ${NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA:+NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA=$NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA} \
    pnpm --filter @rozpisovnik/web build

RUN find frontend/.next -name '*.map' -type f -delete

FROM ${RUNTIME_IMAGE} AS runner

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
