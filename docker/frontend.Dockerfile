# syntax=docker/dockerfile:1.5

FROM node:22-bookworm AS builder
WORKDIR /app
ENV CI=1 \
    YARN_ENABLE_IMMUTABLE_INSTALLS=1 \
    YARN_NODE_LINKER=node-modules \
    NEXT_TELEMETRY_DISABLED=1

COPY package.json yarn.lock .yarnrc.yml ./
COPY .yarn ./.yarn
COPY frontend/package.json frontend/

RUN corepack enable && \
    yarn workspaces focus rozpisovnik-web

COPY frontend frontend

RUN yarn workspace rozpisovnik-web build

FROM node:22-slim AS runner
WORKDIR /app
ENV NODE_ENV=production \
    NEXT_TELEMETRY_DISABLED=1

COPY --from=builder /app/frontend/.next/standalone ./
COPY --from=builder /app/frontend/.next/static ./.next/static
COPY --from=builder /app/frontend/public ./public

EXPOSE 3000

CMD ["node", "server.js"]
