ALTER TABLE `akce` CHANGE `a_jmeno` `a_jmeno` TEXT NOT NULL;
ALTER TABLE `akce` CHANGE `a_kde` `a_kde` TEXT NOT NULL;

ALTER TABLE `aktuality` CHANGE `at_jmeno` `at_jmeno` TEXT NOT NULL;
ALTER TABLE `aktuality` CHANGE `at_preview` `at_preview` TEXT NOT NULL;

ALTER TABLE `dokumenty` DROP INDEX `d_path`;
ALTER TABLE `dokumenty` CHANGE `d_path` `d_path` TEXT NOT NULL;
ALTER TABLE `dokumenty` CHANGE `d_name` `d_name` TEXT NOT NULL;
ALTER TABLE `dokumenty` CHANGE `d_filename` `d_filename` TEXT NOT NULL;

ALTER TABLE `platby_category` CHANGE `pc_name` `pc_name` TEXT NOT NULL;
ALTER TABLE `platby_group` CHANGE `pg_name` `pg_name` TEXT NOT NULL;

ALTER TABLE `rozpis` CHANGE `r_kde` `r_kde` TEXT NOT NULL;

ALTER TABLE `skupiny` CHANGE `s_name` `s_name` TEXT NOT NULL;

ALTER TABLE `users` DROP INDEX `u_jmeno`;
ALTER TABLE `users` DROP INDEX `u_prijmeni`;
ALTER TABLE `users` CHANGE `u_jmeno` `u_jmeno` TEXT NOT NULL;
ALTER TABLE `users` CHANGE `u_prijmeni` `u_prijmeni` TEXT NOT NULL;
ALTER TABLE `users` CHANGE `u_email` `u_email` TEXT NOT NULL;
