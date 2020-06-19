# Hipsterfy

Figure out which of your Spotify artist preferences are most hipster.

## Usage

1. Authorize Hipsterfy to access your Spotify account.
2. Hipsterfy generates a "friend code" for you.
3. Share your "friend code" with your friends.
4. When submitting a friend code, Hipsterfy shows the artists you both follow, ordered by least-popular-first.

## Implementation

1. Get user's followed artists: https://developer.spotify.com/documentation/web-api/reference/follow/get-followed/
2. Get user's saved tracks: https://developer.spotify.com/documentation/web-api/reference/library/get-users-saved-tracks/
3. Get anonymous Spotify bearer token: https://open.spotify.com/get_access_token?reason=transport&productType=web_player
4. Get Spotify artist insights: `curl 'https://spclient.wg.spotify.com/open-backend-2/v1/artists/{artist_id}' -H 'authorization: Bearer XXXX`

## Development

### Running on raw metal

#### Create the database

We use provide a Dockerfile for the database image for convenience. You may also choose to run your own Postgres instance. The schema migration is at [`./schema.sql`](./schema.sql).

To create the Docker image database:

```bash
# Build the database.
sudo docker build -f ./images/hipsterfy-db/Dockerfile -t hipsterfy-db .

# Start the database initially. Make sure to set environment variables correctly;
# these are used to initialize the database on first run.
sudo docker run --name hipsterfy-db -p 5432:5432 -e POSTGRES_USER=hipsterfy -e POSTGRES_PASSWORD=hunter2 hipsterfy-db
```

In the the future, you can start the database with:

```bash
sudo docker start hipsterfy-db
```

#### Build and run the server

Make sure to populate the flags with your own:

- Server host and port
- Postgres database connection string
- Spotify app client ID and secret

```bash
cabal-fmt --inplace hipsterfy.cabal
cabal run hipsterfy -- --host http://localhost --port 8000 --db 'postgresql://hipsterfy:hunter2@localhost:5432' --client_id XXXX --client_secret XXXX
```

### Running with `docker-compose`

Docker Compose will start both containers for you.

```bash
sudo docker-compose -p hipsterfy-dev up --build
```
