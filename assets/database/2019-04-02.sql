CREATE TABLE `video_source` (
    `vs_id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    `vs_url` TEXT CHARACTER SET utf8 NOT NULL,
    `vs_title` TEXT CHARACTER SET utf8 NULL,
    `vs_description` TEXT CHARACTER SET utf8 NULL,
    `vs_created_at` DATETIME NOT NULL,
    `vs_last_checked` TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
);

ALTER TABLE `video` DROP COLUMN `v_date`;
ALTER TABLE `video` CHANGE `v_uri` `v_uri` TEXT CHARACTER SET utf8 NOT NULL;
ALTER TABLE `video` CHANGE `v_name` `v_title` TEXT CHARACTER SET utf8 NOT NULL AFTER v_uri;
ALTER TABLE `video` ADD COLUMN `v_author` TEXT CHARACTER SET utf8 NOT NULL AFTER v_title;
ALTER TABLE `video` CHANGE `v_text` `v_description` TEXT CHARACTER SET utf8 NOT NULL AFTER v_author;
ALTER TABLE `video` CHANGE `v_playlist` `v_playlist` TEXT CHARACTER SET utf8 NULL AFTER v_description;
ALTER TABLE `video` ADD COLUMN `v_created_at` DATETIME NOT NULL AFTER v_playlist;
ALTER TABLE `video` CHANGE `v_timestamp` `v_updated_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP AFTER v_created_at;
UPDATE `video` SET `v_created_at` = `v_updated_at`;
UPDATE `video` SET `v_playlist` = NULL;
