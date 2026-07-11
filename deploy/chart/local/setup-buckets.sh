#!/usr/bin/env sh
set -eu

bucket="${S3_BUCKET:-olymp}"
filer_host="${SEAWEED_FILER_HOST:-127.0.0.1}"
filer_port="${SEAWEED_FILER_PORT:-6301}"
filer_grpc_port="${SEAWEED_FILER_GRPC_PORT:-6306}"
filer_http="${filer_host}:${filer_port}"
filer_shell="${filer_http}.${filer_grpc_port}"
master_host="${SEAWEED_MASTER_HOST:-127.0.0.1}"
master_port="${SEAWEED_MASTER_PORT:-6302}"
master_grpc_port="${SEAWEED_MASTER_GRPC_PORT:-6304}"
master_shell="${master_host}:${master_port}.${master_grpc_port}"
ready_url="http://${filer_http}/"

attempt=1
while [ "$attempt" -le 60 ]; do
  if curl -fsS "$ready_url" >/dev/null 2>&1; then
    if output=$(printf 's3.bucket.create -name %s\n' "$bucket" | weed shell -master="$master_shell" -filer="$filer_shell" 2>&1); then
      printf 'SeaweedFS bucket ready: %s\n' "$bucket"
      exit 0
    fi

    printf '%s\n' "$output" >&2
    printf 'SeaweedFS bucket setup failed for %s\n' "$bucket" >&2
    exit 1
  fi

  attempt=$((attempt + 1))
  sleep 1
done

printf 'SeaweedFS filer did not become ready at %s\n' "$ready_url" >&2
exit 1
