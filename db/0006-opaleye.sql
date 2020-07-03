ALTER TABLE hipsterfy_user ALTER COLUMN spotify_user_id SET NOT NULL;
ALTER TABLE hipsterfy_user ALTER COLUMN spotify_user_name SET NOT NULL;
ALTER TABLE hipsterfy_user ALTER COLUMN spotify_access_token SET NOT NULL;
ALTER TABLE hipsterfy_user ALTER COLUMN spotify_access_token_expiration SET NOT NULL;
ALTER TABLE hipsterfy_user ALTER COLUMN spotify_refresh_token SET NOT NULL;

ALTER TABLE hipsterfy_user ALTER COLUMN created_at SET DEFAULT NOW();
ALTER TABLE spotify_artist ALTER COLUMN created_at SET DEFAULT NOW();
ALTER TABLE spotify_oauth_request ALTER COLUMN created_at SET DEFAULT NOW();
ALTER TABLE spotify_artist_listeners ALTER COLUMN created_at SET DEFAULT NOW();
