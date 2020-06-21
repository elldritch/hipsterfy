ALTER TABLE hipsterfy_user ADD COLUMN created_at TIMESTAMPTZ NOT NULL DEFAULT now();
ALTER TABLE hipsterfy_user ALTER COLUMN created_at DROP DEFAULT;

ALTER TABLE spotify_oauth_request ADD COLUMN created_at TIMESTAMPTZ NOT NULL DEFAULT now();
ALTER TABLE spotify_oauth_request ALTER COLUMN created_at DROP DEFAULT;

ALTER TABLE hipsterfy_user_session ADD COLUMN created_at TIMESTAMPTZ NOT NULL DEFAULT now();
ALTER TABLE hipsterfy_user_session ALTER COLUMN created_at DROP DEFAULT;

ALTER TABLE spotify_artist ADD COLUMN created_at TIMESTAMPTZ NOT NULL DEFAULT now();
ALTER TABLE spotify_artist ALTER COLUMN created_at DROP DEFAULT;

ALTER TABLE spotify_artist_listeners ALTER COLUMN timestamp TYPE TIMESTAMPTZ;
ALTER TABLE spotify_artist_listeners RENAME COLUMN timestamp TO created_at;
