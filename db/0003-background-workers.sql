CREATE TABLE job (
  id SERIAL PRIMARY KEY,

  created_at    TIMESTAMPTZ NOT NULL,
  claimed_at    TIMESTAMPTZ,
  claimed_until TIMESTAMPTZ,
  claimed_by    TEXT,
  status        TEXT NOT NULL,

  name TEXT  NOT NULL,
  args JSONB NOT NULL,
  info JSONB NOT NULL
);
