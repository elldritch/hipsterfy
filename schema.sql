CREATE TABLE hipsterfy_user (
  id            SERIAL PRIMARY KEY,
  friend_code   TEXT   NOT NULL UNIQUE,

  spotify_user_id                 TEXT UNIQUE,
  spotify_access_token            TEXT,
  spotify_access_token_expiration TIMESTAMPTZ,
  spotify_refresh_token           TEXT
);

CREATE TABLE spotify_oauth_request (
  id           SERIAL PRIMARY KEY,
  oauth2_state TEXT   NOT NULL UNIQUE
);

CREATE TABLE hipsterfy_user_session (
  id            SERIAL PRIMARY KEY,
  user_id       INT    NOT NULL REFERENCES hipsterfy_user(id),
  cookie_secret TEXT   NOT NULL UNIQUE
);
