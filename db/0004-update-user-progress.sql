ALTER TABLE hipsterfy_user ADD COLUMN follows_currently_updating BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE hipsterfy_user ALTER COLUMN follows_currently_updating DROP DEFAULT;

ALTER TABLE hipsterfy_user ADD COLUMN follows_last_update_start TIMESTAMPTZ NOT NULL DEFAULT now();
ALTER TABLE hipsterfy_user ALTER COLUMN follows_last_update_start DROP DEFAULT;

ALTER TABLE hipsterfy_user ADD COLUMN follows_current_update_total INT;
