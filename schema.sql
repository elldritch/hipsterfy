CREATE TABLE hipsterfy_user (
  id            SERIAL PRIMARY KEY,
  friend_code   TEXT   NOT NULL,
  oauth2_secret TEXT   NOT NULL,

  spotify_user_id                 TEXT,
  spotify_access_token            TEXT,
  spotify_access_token_expiration TIMESTAMP,
  spotify_refresh_token           TEXT
);
