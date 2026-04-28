backend: pnpm -C backend run start
web: pnpm -C frontend run dev
migrate: pnpm graphile-migrate watch
worker: pnpm -C worker run start -- -j 5

schema: trap 'pkill -CHLD tmux' 0; pnpm run schema -- --watch
schema-starlet: trap 'pkill -CHLD tmux' 0; npm run schema-starlet -- --watch
sql-worker: pnpm -C worker run sql:generate -- --watch

postgrest: PGRST_SERVER_PORT=${PORT} postgrest
