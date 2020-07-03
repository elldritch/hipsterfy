# Build application.
FROM docker.pkg.github.com/liftm/hipsterfy/hipsterfy-base:sha-15c9c5c AS builder

# Build the application.
RUN mkdir -p /artifacts
COPY . /build
RUN cabal install --installdir /artifacts --install-method copy

# Post-process executables.
WORKDIR /artifacts
RUN ls -1 | xargs strip
RUN ls -1 | xargs upx

# Show built artifacts.
RUN ls -lh

###############################################################################

# Run application.
FROM debian:10.4

# Install system runtime dependencies.
RUN apt-get update
RUN apt-get install -y libpq5 ca-certificates

# Copy build artifact from a builder stage
COPY --from=builder /artifacts /usr/local/bin

ENTRYPOINT ["/usr/local/bin/hipsterfy"]
