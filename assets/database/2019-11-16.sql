CREATE TABLE `session` (
    `ss_id` VARCHAR(128) NOT NULL PRIMARY KEY,
    `ss_data` BLOB NOT NULL,
    `ss_updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `ss_lifetime` BIGINT NOT NULL
);
