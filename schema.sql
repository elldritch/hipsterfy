CREATE TABLE hipsterfy_user (
  id            SERIAL PRIMARY KEY,
  friend_code   TEXT   NOT NULL UNIQUE,
  oauth2_secret TEXT   NOT NULL UNIQUE,

  spotify_user_id                 TEXT UNIQUE,
  spotify_access_token            TEXT,
  spotify_access_token_expiration TIMESTAMP,
  spotify_refresh_token           TEXT
);

-- TODO: add proper session management.
