ALTER TABLE hipsterfy_user DROP COLUMN follows_currently_updating;
ALTER TABLE hipsterfy_user DROP COLUMN follows_last_update_start;
ALTER TABLE hipsterfy_user DROP COLUMN follows_current_update_total;

ALTER TABLE hipsterfy_user ADD COLUMN last_update_job_submitted TIMESTAMPTZ;
ALTER TABLE hipsterfy_user ADD COLUMN last_update_job_completed TIMESTAMPTZ;

ALTER TABLE spotify_artist ADD COLUMN last_update_job_submitted TIMESTAMPTZ;
ALTER TABLE spotify_artist ADD COLUMN last_update_job_completed TIMESTAMPTZ;
