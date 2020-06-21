ALTER TABLE hipsterfy_user ADD COLUMN created_at TIMESTAMPTZ NOT NULL;

ALTER TABLE spotify_oauth_request ADD COLUMN created_at TIMESTAMPTZ NOT NULL;

ALTER TABLE hipsterfy_user_session ADD COLUMN created_at TIMESTAMPTZ NOT NULL;

ALTER TABLE spotify_artist ADD COLUMN created_at TIMESTAMPTZ NOT NULL;

ALTER TABLE spotify_artist_listeners ALTER COLUMN timestamp TYPE TIMESTAMPTZ NOT NULL;
ALTER TABLE spotify_artist_listeners RENAME COLUMN timestamp TO created_at;
