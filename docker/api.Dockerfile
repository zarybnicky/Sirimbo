# syntax=docker/dockerfile:1.5

FROM node:22-bookworm AS builder
WORKDIR /app
ENV CI=1 \
    YARN_ENABLE_IMMUTABLE_INSTALLS=1 \
    YARN_NODE_LINKER=node-modules

COPY package.json yarn.lock .yarnrc.yml ./
COPY .yarn ./.yarn
COPY backend/package.json backend/

RUN corepack enable && \
    yarn workspaces focus rozpisovnik-api

COPY backend backend

RUN yarn workspace rozpisovnik-api build && \
    yarn workspaces focus rozpisovnik-api --production

FROM node:22-slim AS runner
WORKDIR /app
ENV NODE_ENV=production \
    PORT=5000 \
    PATH=/app/node_modules/.bin:$PATH

COPY --from=builder /app/package.json ./package.json
COPY --from=builder /app/yarn.lock ./yarn.lock
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/backend ./backend

EXPOSE 5000

CMD ["node", "backend/dist/index.cjs"]
