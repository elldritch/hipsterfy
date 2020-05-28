# Hipsterfy

Figure out which of your Spotify artist preferences are most hipster.

## Usage

1. Authorize Hipsterfy to access your Spotify account.
2. Hipsterfy generates a "friend code" for you.
3. Share your "friend code" with your friends.
4. When submitting a friend code, Hipsterfy shows the artists you both follow, ordered by least-popular-first.

## Implementation

1. Get user's followed artists: https://developer.spotify.com/documentation/web-api/reference/follow/get-followed/
2. Get user's saved tracks: https://developer.spotify.com/documentation/web-api/reference/library/get-users-saved-tracks/data
3. Get anonymous Spotify bearer token: https://open.spotify.com/get_access_token?reason=transport&productType=web_player
4. Get Spotify artist insights: `curl 'https://spclient.wg.spotify.com/open-backend-2/v1/artists/62GoYifV4njTdvS8lD2yYT' -H 'authorization: Bearer XXXX`

## Development

```
sudo docker run --name hipsterfy-db -p 5432:5432 -e POSTGRES_USER=hipsterfy -e POSTGRES_PASSWORD=hunter2 postgres:12.3-alpine
cabal-fmt --inplace hipsterfy.cabal && cabal run hipsterfy -- --host http://localhost --port 8000 --db 'postgresql://hipsterfy:hunter2@localhost:5432' --client_id XXXX --client_secret XXXX
```
