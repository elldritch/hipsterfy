# Build dependencies.
FROM haskell:8.8.3-buster AS builder

# Update dependency indexes.
RUN apt-get update
RUN cabal update

# Install system dependencies.
RUN apt-get install -y libpq-dev upx-ucl

# Install application dependencies.
# This is a separate image to improve caching.
WORKDIR /build
COPY *.cabal cabal.project /build/

# Work around https://github.com/haskell/cabal/pull/6927
RUN cabal build --dependencies-only || true
RUN cd dist-newstyle/src/tracing-d51abce8e903424f/ && git checkout fix-missing-content-type-jaeger
RUN cabal build --dependencies-only
