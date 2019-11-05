FROM fpco/stack-build:lts-14.12
RUN stack install ghcid
WORKDIR /local
CMD ghcid -T main -c "stack ghci --test --bench --ghci-options=-fobject-code --ghci-options=-fno-break-on-exception --ghci-options=-fno-break-on-error --ghci-options=-v1 --ghci-options=-ferror-spans --ghci-options=-j --allow-different-user --no-docker" --restart=olymp-api.cabal -s=":set args --port 3000"
