# Hipsterfy

Find out which Spotify hipsters you and your friends both like.

## Usage

1. Authorize Hipsterfy to access your Spotify account.
2. Hipsterfy generates a "friend code" for you.
3. Share your "friend code" with your friends.
4. When submitting a friend code, Hipsterfy shows the artists you both follow, ordered by least-popular-first.

Your followed artists are a combination of artists you've followed, artists of tracks you've saved, and artists of albums you've saved.

## Implementation

1. Get user's followed artists: https://developer.spotify.com/documentation/web-api/reference/follow/get-followed/
2. Get user's saved tracks: https://developer.spotify.com/documentation/web-api/reference/library/get-users-saved-tracks/
3. Get user's saved albums: https://developer.spotify.com/documentation/web-api/reference/library/get-users-saved-albums/
4. Get anonymous Spotify bearer token: https://open.spotify.com/get_access_token?reason=transport&productType=web_player
5. Get Spotify artist insights (this undocumented API is how monthly listeners are retrieved - otherwise, we'd need to crawl) for each artist: `curl 'https://spclient.wg.spotify.com/open-backend-2/v1/artists/{artist_id}' -H 'authorization: Bearer XXXX`

## Development

### Running on raw metal

#### Create and run the database

Hipsterfy provides a Dockerfile for the database image for convenience. You may also choose to run your own Postgres instance. The schema migrations are in [`./db`](./db).

To create the database container using the Docker image:

```bash
# Build the database.
sudo docker build -f ./images/hipsterfy-db/Dockerfile -t hipsterfy-db .

# Start the database initially.
# Make sure to set environment variables correctly; these
# are used to initialize the database on first run.
sudo docker run --name hipsterfy-db \
  -p 5432:5432 \
  -e POSTGRES_USER=hipsterfy \
  -e POSTGRES_PASSWORD=hunter2 \
  hipsterfy-db
```

After the database container has been created, you can start it with:

```bash
sudo docker start hipsterfy-db
```

#### Run the job queue server

Hipsterfy uses [Faktory](https://github.com/contribsys/faktory) as a job queue server.

To create the job queue container:

```bash
sudo docker run --name hipsterfy-jobqueue \
  -p 7419:7419 \
  -p 7420:7420 \
  -e FAKTORY_PASSWORD=hunter2 \
  contribsys/faktory:1.4.0
```

After the job queue container has been created, you can start it with:

```bash
sudo docker start hipsterfy-jobqueue
```

#### Build and run the web server

Make sure to populate the flags with your own:

- Server host and port. (Make sure this is the same as your redirect URI in Spotify, including protocol! Otherwise, Spotify will refuse to redirect authorizing users, or will redirect incorrectly.)
- Spotify app client ID and secret.
- Postgres database connection string.
- Faktory host, port, and password.

```bash
cabal run hipsterfy -- \
  --host http://localhost \
  --port 8000 \
  --client_id XXXX \
  --client_secret XXXX \
  --db 'postgresql://hipsterfy:hunter2@localhost:5432' \
  --faktory_host localhost \
  --faktory_port 7419 \
  --faktory_password hunter2
```

#### Build and run the worker

Make sure to populate the flags with your own:

- Spotify app client ID and secret.
- Postgres database connection string.
- Faktory host, port, and password.

```bash
cabal run hipsterfy-worker -- \
  --client_id ea880280c7cc4427912e0ef932a57b68 \
  --client_secret 93206bec68d940e791f7d1cf26eeedc5 \
  --db 'postgresql://hipsterfy:hunter2@localhost:5432' \
  --faktory_host localhost \
  --faktory_port 7419 \
  --faktory_password hunter2
```

### Running with `docker-compose`

Docker Compose will start all containers for you. Make sure to set environment variables for configuration.

```bash
export HIPSTERFY_ADDR_HOST=http://localhost
export HIPSTERFY_ADDR_PORT=8000
export HIPSTERFY_SPOTIFY_CLIENT_ID=XXXX
export HIPSTERFY_SPOTIFY_CLIENT_SECRET=XXXX
export HIPSTERFY_DB_USER=hipsterfy
export HIPSTERFY_DB_PASSWORD=hunter2
export HIPSTERFY_JOBQUEUE_PASSWORD=hunter2
sudo docker-compose -p hipsterfy-dev up --build
```

Docker Compose is useful for fully tearing down and setting up new instances for testing. You can completely tear down a Docker Compose deployment with:

```
sudo docker-compose -p hipsterfy-dev down --volumes
```

### Generating documentation

Run Haddock to generate documentation for both the current project and its dependencies.

```bash
cabal haddock --haddock-all --enable-documentation
```

The output will have a line similar to:

```bash
Documentation created:
.../hipsterfy/dist-newstyle/build/x86_64-linux/ghc-8.8.3/hipsterfy-0.1.0.0/noopt/doc/html/hipsterfy/index.html
```

Open this file in your browser to view the documentation.

### Running tests

Automated integration tests require user access tokens in order to test that they work.

```bash
cabal test --test-show-details=streaming --test-options='--access_token=XXXX'
```

#### Retrieving an access token

Run `hipsterfy-tools get-access-token` to retrieve a user access token for testing.

```bash
cabal run hipsterfy-tools -- get-access-token --db 'postgresql://hipsterfy:hunter2@localhost:5432' --client_id XXXX --client_secret XXXX USER_SPOTIFY_ID
```

### Formatting code

For code, use [Ormolu](https://github.com/tweag/ormolu). For `hipsterfy.cabal`, use [`cabal-fmt`](https://github.com/phadej/cabal-fmt).
