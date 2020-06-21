CREATE TABLE hipsterfy_user_spotify_artist_follow (
  user_id           INT NOT NULL REFERENCES hipsterfy_user(id),
  spotify_artist_id INT NOT NULL REFERENCES spotify_artist(id),
  PRIMARY KEY (user_id, spotify_artist_id)
);
