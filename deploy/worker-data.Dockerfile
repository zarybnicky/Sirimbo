# syntax=docker/dockerfile:1

FROM nixos/nix:2.24.11 AS worker-data
WORKDIR /src
COPY flake.nix flake.lock ./
COPY deploy/*-cache.nix deploy/*-cache.sql deploy/
ENV NIX_CONFIG="experimental-features = nix-command flakes"
RUN mkdir /data && nix build .#ruian-address-cache --out-link /tmp/ruian \
    && cp /tmp/ruian /data/ruian.duckdb
RUN nix build .#geonames-city-cache --out-link /tmp/geonames \
    && cp /tmp/geonames /data/geonames.duckdb

FROM scratch
COPY --from=worker-data /data/ /data/
