CREATE TABLE `video_source` (
    `vs_id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    `vs_url` TEXT CHARACTER SET utf8 NOT NULL,
    `vs_title` TEXT CHARACTER SET utf8 NULL,
    `vs_description` TEXT CHARACTER SET utf8 NULL,
    `vs_created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `vs_last_checked` TIMESTAMP NULL
);
