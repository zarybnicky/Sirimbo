#!/usr/bin/env bash

set -eux
cd "$(dirname "${BASH_SOURCE[0]}")"

:> worker/pnpm-deps-hash.txt # Clear hash to trigger rebuild
BUILD_LOG="$(mktemp)"
nix build -L .#rozpisovnik-worker.pnpmDeps 2>&1 | tee "$BUILD_LOG"
grep -oP 'got: +\K\S+' "$BUILD_LOG" > worker/pnpm-deps-hash.txt

:> backend/pnpm-deps-hash.txt # Clear hash to trigger rebuild
BUILD_LOG="$(mktemp)"
nix build -L .#rozpisovnik-api.pnpmDeps 2>&1 | tee "$BUILD_LOG"
grep -oP 'got: +\K\S+' "$BUILD_LOG" > backend/pnpm-deps-hash.txt
