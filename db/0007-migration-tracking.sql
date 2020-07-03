CREATE TABLE migrations (
  name       TEXT        PRIMARY KEY,
  created_at TIMESTAMPTZ NOT NULL
);

INSERT INTO migrations (name, created_at) VALUES ('0007-migration-tracking.sql', NOW());
