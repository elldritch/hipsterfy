CREATE TABLE hipsterfy_user (
  id            SERIAL PRIMARY KEY,
  friend_code   TEXT   NOT NULL UNIQUE,

  spotify_user_id                 TEXT UNIQUE,
  spotify_user_name               TEXT,
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

CREATE TABLE spotify_artist (
  id SERIAL PRIMARY KEY,

  name              TEXT   NOT NULL,
  spotify_artist_id TEXT   NOT NULL UNIQUE,
  spotify_url       TEXT   NOT NULL,
  biography         TEXT   NOT NULL,
  autobiography     TEXT   NOT NULL,
  followers         INT    NOT NULL,
  genres            TEXT[] NOT NULL,
  popularity        INT    NOT NULL,

  last_updated TIMESTAMPTZ NOT NULL
);

CREATE TABLE spotify_artist_listeners (
  id SERIAL PRIMARY KEY,

  spotify_artist_id INT NOT NULL REFERENCES spotify_artist(id),
  timestamp         DATE NOT NULL,
  monthly_listeners INT NOT NULL

  -- TODO: we can segment this per-city as well. Does the sum of all cities equal total listeners?
);
