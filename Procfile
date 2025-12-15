backend: pnpm -C backend run start
web: pnpm -C frontend run dev
migrate: pnpm graphile-migrate watch
worker: pnpm -C worker run start -- -j 5

schema: exec npm run schema -- --watch
schema-starlet: exec npm run schema-starlet -- --watch
sql-worker: pnpm -C worker run sql:generate -- --watch
