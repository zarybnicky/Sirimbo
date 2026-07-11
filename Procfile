migrate: pnpm graphile-migrate watch
web: pnpm -C frontend run dev
backend: pnpm -C backend run start
seaweed: weed server -s3 -dir=./deploy/local/seaweed -ip=${S3_HOST} -ip.bind=${S3_HOST} -master.port=6302 -master.port.grpc=6304 -volume.port=6303 -volume.port.grpc=6305 -filer.port=6301 -filer.port.grpc=6306 -s3.port=${S3_PORT} -s3.port.grpc=6307 -s3.config=./deploy/local/s3conf.json
mailpit: mailpit --database ./deploy/local/mailpit/mailpit.db --listen ${SMTP_HOST}:6400 --smtp ${SMTP_HOST}:${SMTP_PORT} --disable-version-check
seaweed-setup: ./deploy/local/setup-buckets.sh

worker: pnpm -C worker run start -- -j 5
schema: trap 'pkill -CHLD tmux' 0; pnpm run schema -- --watch
schema-starlet: trap 'pkill -CHLD tmux' 0; npm run schema-starlet -- --watch
sql-worker: pnpm -C worker run sql:generate --watch

postgrest: PGRST_SERVER_PORT=5700 postgrest
