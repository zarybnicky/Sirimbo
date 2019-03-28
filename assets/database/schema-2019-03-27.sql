--
-- Table structure for table `akce`
--
CREATE TABLE `akce` (
  `a_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `a_jmeno` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `a_kde` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `a_info` text COLLATE cp1250_czech_cs NOT NULL,
  `a_od` date NOT NULL,
  `a_do` date NOT NULL,
  `a_kapacita` int(11) NOT NULL DEFAULT '0',
  `a_dokumenty` text COLLATE cp1250_czech_cs NOT NULL,
  `a_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `a_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `a_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`a_id`)
) ENGINE=MyISAM AUTO_INCREMENT=45 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `akce_item`
--
CREATE TABLE `akce_item` (
  `ai_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ai_id_rodic` int(11) unsigned NOT NULL,
  `ai_user` int(11) unsigned DEFAULT NULL,
  `ai_rok_narozeni` smallint(4) NOT NULL,
  PRIMARY KEY (`ai_id`)
) ENGINE=MyISAM AUTO_INCREMENT=1068 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `aktuality`
--
CREATE TABLE `aktuality` (
  `at_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `at_kdo` int(11) unsigned NOT NULL,
  `at_kat` enum('1','2','3') COLLATE cp1250_czech_cs NOT NULL,
  `at_jmeno` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `at_text` text COLLATE cp1250_czech_cs NOT NULL,
  `at_preview` varchar(200) COLLATE cp1250_czech_cs NOT NULL,
  `at_foto` int(11) unsigned NOT NULL,
  `at_foto_main` int(11) unsigned NOT NULL,
  `at_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `at_timestamp_add` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`at_id`),
  KEY `at_timestamp_add` (`at_timestamp_add`)
) ENGINE=MyISAM AUTO_INCREMENT=417 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `aktuality_foto`
--
CREATE TABLE `aktuality_foto` (
  `af_id` int(11) NOT NULL AUTO_INCREMENT,
  `af_id_rodic` int(11) NOT NULL,
  `af_id_foto` int(11) NOT NULL,
  `af_path` text COLLATE cp1250_czech_cs NOT NULL,
  PRIMARY KEY (`af_id`),
  KEY `af_id_rodic` (`af_id_rodic`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `dokumenty`
--
CREATE TABLE `dokumenty` (
  `d_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `d_path` varchar(50) COLLATE cp1250_czech_cs NOT NULL,
  `d_name` varchar(80) COLLATE cp1250_czech_cs NOT NULL,
  `d_filename` varchar(80) COLLATE cp1250_czech_cs NOT NULL,
  `d_kategorie` smallint(3) NOT NULL,
  `d_kdo` int(11) unsigned NOT NULL,
  `d_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`d_id`),
  UNIQUE KEY `d_path` (`d_path`)
) ENGINE=MyISAM AUTO_INCREMENT=70 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `galerie_dir`
--
CREATE TABLE `galerie_dir` (
  `gd_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `gd_id_rodic` int(11) unsigned NOT NULL,
  `gd_name` text COLLATE cp1250_czech_cs NOT NULL,
  `gd_level` smallint(6) NOT NULL DEFAULT '1',
  `gd_path` text COLLATE cp1250_czech_cs NOT NULL,
  `gd_hidden` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`gd_id`),
  KEY `gd_id_rodic` (`gd_id_rodic`)
) ENGINE=MyISAM AUTO_INCREMENT=246 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `galerie_foto`
--
CREATE TABLE `galerie_foto` (
  `gf_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `gf_id_rodic` int(11) unsigned NOT NULL,
  `gf_name` text COLLATE cp1250_czech_cs NOT NULL,
  `gf_path` text COLLATE cp1250_czech_cs NOT NULL,
  `gf_kdo` int(11) unsigned NOT NULL,
  `gf_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`gf_id`),
  KEY `gf_id_rodic` (`gf_id_rodic`)
) ENGINE=MyISAM AUTO_INCREMENT=10049 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `inzerce`
--
CREATE TABLE `inzerce` (
  `i_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `i_kat` enum('1','2','3','4') COLLATE cp1250_czech_cs NOT NULL,
  `i_reg` int(11) unsigned NOT NULL,
  `i_jmeno` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `i_prijmeni` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `i_nadpis` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `i_text` text COLLATE cp1250_czech_cs NOT NULL,
  `i_foto` blob NOT NULL,
  `i_od` date NOT NULL,
  `i_do` date NOT NULL,
  `i_pass` char(20) COLLATE cp1250_czech_cs NOT NULL,
  `i_confirmed` enum('0','1') COLLATE cp1250_czech_cs NOT NULL,
  `i_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL,
  `i_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`i_id`)
) ENGINE=MyISAM AUTO_INCREMENT=36 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `nabidka`
--
CREATE TABLE `nabidka` (
  `n_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `n_trener` int(11) unsigned NOT NULL,
  `n_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `n_max_pocet_hod` int(11) NOT NULL DEFAULT '0',
  `n_od` date NOT NULL,
  `n_do` date NOT NULL,
  `n_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `n_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `n_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`n_id`),
  KEY `n_trener` (`n_trener`)
) ENGINE=MyISAM AUTO_INCREMENT=483 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `nabidka_item`
--
CREATE TABLE `nabidka_item` (
  `ni_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ni_id_rodic` int(11) unsigned NOT NULL,
  `ni_partner` int(11) unsigned NOT NULL,
  `ni_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `ni_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`ni_id`),
  UNIQUE KEY `ni_id_rodic` (`ni_id_rodic`,`ni_partner`)
) ENGINE=MyISAM AUTO_INCREMENT=2683 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `novinky`
--
CREATE TABLE `novinky` (
  `no_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `no_id_user` int(11) NOT NULL,
  `no_text` text COLLATE cp1250_czech_cs NOT NULL,
  `no_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`no_id`)
) ENGINE=MyISAM AUTO_INCREMENT=3342 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `parameters`
--
CREATE TABLE `parameters` (
  `pa_name` varchar(40) NOT NULL,
  `pa_value` text NOT NULL,
  PRIMARY KEY (`pa_name`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250;

--
-- Table structure for table `pary`
--
CREATE TABLE `pary` (
  `p_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `p_id_partner` int(11) unsigned NOT NULL,
  `p_id_partnerka` int(11) unsigned NOT NULL DEFAULT '0',
  `p_stt_trida` enum('Z','H','D','C','B','A','M') COLLATE cp1250_czech_cs NOT NULL DEFAULT 'Z',
  `p_stt_body` smallint(5) unsigned NOT NULL DEFAULT '0',
  `p_stt_finale` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `p_lat_trida` enum('Z','H','D','C','B','A','M') COLLATE cp1250_czech_cs NOT NULL DEFAULT 'Z',
  `p_lat_body` smallint(5) unsigned NOT NULL DEFAULT '0',
  `p_lat_finale` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `p_hodnoceni` mediumint(8) unsigned NOT NULL DEFAULT '0',
  `p_archiv` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `p_timestamp_add` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `p_timestamp_archive` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`p_id`),
  KEY `p_hodnoceni` (`p_hodnoceni`)
) ENGINE=MyISAM AUTO_INCREMENT=1935 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `pary_navrh`
--
CREATE TABLE `pary_navrh` (
  `pn_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `pn_navrhl` int(11) unsigned NOT NULL,
  `pn_partner` int(11) unsigned NOT NULL,
  `pn_partnerka` int(11) unsigned NOT NULL,
  PRIMARY KEY (`pn_id`)
) ENGINE=MyISAM AUTO_INCREMENT=139 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `permissions`
--
CREATE TABLE `permissions` (
  `pe_id` int(11) NOT NULL AUTO_INCREMENT,
  `pe_name` text COLLATE cp1250_czech_cs NOT NULL,
  `pe_description` text COLLATE cp1250_czech_cs NOT NULL,
  `pe_akce` int(2) NOT NULL,
  `pe_aktuality` int(2) NOT NULL,
  `pe_ankety` int(2) NOT NULL,
  `pe_dokumenty` int(2) NOT NULL,
  `pe_galerie` int(2) NOT NULL,
  `pe_inzerce` int(2) NOT NULL,
  `pe_konzole` int(2) NOT NULL,
  `pe_nabidka` int(2) NOT NULL,
  `pe_nastenka` int(2) NOT NULL,
  `pe_novinky` int(2) NOT NULL,
  `pe_pary` int(2) NOT NULL,
  `pe_platby` int(2) NOT NULL,
  `pe_permissions` int(2) NOT NULL,
  `pe_rozpis` int(2) NOT NULL,
  `pe_skupiny` int(2) NOT NULL,
  `pe_users` int(2) NOT NULL,
  `pe_main` int(2) NOT NULL,
  PRIMARY KEY (`pe_id`)
) ENGINE=MyISAM AUTO_INCREMENT=11 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `platby_category`
--
CREATE TABLE `platby_category` (
  `pc_id` int(11) NOT NULL AUTO_INCREMENT,
  `pc_name` varchar(50) COLLATE cp1250_czech_cs NOT NULL,
  `pc_symbol` int(11) DEFAULT NULL,
  `pc_amount` decimal(10,2) NOT NULL,
  `pc_date_due` date NOT NULL,
  `pc_valid_from` date NOT NULL,
  `pc_valid_to` date NOT NULL,
  `pc_use_base` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `pc_use_prefix` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `pc_archive` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `pc_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '1',
  PRIMARY KEY (`pc_id`),
  UNIQUE KEY `pc_symbol` (`pc_symbol`)
) ENGINE=MyISAM AUTO_INCREMENT=91 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `platby_category_group`
--
CREATE TABLE `platby_category_group` (
  `pcg_id` int(11) NOT NULL AUTO_INCREMENT,
  `pcg_id_group` int(11) NOT NULL,
  `pcg_id_category` int(11) NOT NULL,
  PRIMARY KEY (`pcg_id`),
  UNIQUE KEY `pcg_id_group` (`pcg_id_group`,`pcg_id_category`)
) ENGINE=MyISAM AUTO_INCREMENT=765 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `platby_group`
--
CREATE TABLE `platby_group` (
  `pg_id` int(11) NOT NULL AUTO_INCREMENT,
  `pg_type` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `pg_name` varchar(50) COLLATE cp1250_czech_cs NOT NULL,
  `pg_description` text COLLATE cp1250_czech_cs NOT NULL,
  `pg_base` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`pg_id`)
) ENGINE=MyISAM AUTO_INCREMENT=53 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `platby_group_skupina`
--
CREATE TABLE `platby_group_skupina` (
  `pgs_id` int(11) NOT NULL AUTO_INCREMENT,
  `pgs_id_skupina` int(11) NOT NULL,
  `pgs_id_group` int(11) NOT NULL,
  PRIMARY KEY (`pgs_id`),
  UNIQUE KEY `pgs_id_skupina` (`pgs_id_skupina`,`pgs_id_group`)
) ENGINE=MyISAM AUTO_INCREMENT=159 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `platby_item`
--
CREATE TABLE `platby_item` (
  `pi_id` int(11) NOT NULL AUTO_INCREMENT,
  `pi_id_user` int(11) NOT NULL,
  `pi_id_category` int(11) NOT NULL,
  `pi_id_raw` int(11) DEFAULT NULL,
  `pi_amount` decimal(10,2) NOT NULL,
  `pi_date` date NOT NULL,
  `pi_prefix` int(4) NOT NULL DEFAULT '2000',
  PRIMARY KEY (`pi_id`),
  UNIQUE KEY `pi_id_raw` (`pi_id_raw`)
) ENGINE=MyISAM AUTO_INCREMENT=1811 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `platby_raw`
--
CREATE TABLE `platby_raw` (
  `pr_id` int(11) NOT NULL AUTO_INCREMENT,
  `pr_raw` blob NOT NULL,
  `pr_hash` varchar(32) COLLATE cp1250_czech_cs NOT NULL,
  `pr_sorted` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `pr_discarded` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`pr_id`),
  UNIQUE KEY `pr_hash` (`pr_hash`)
) ENGINE=MyISAM AUTO_INCREMENT=3258 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `rozpis`
--
CREATE TABLE `rozpis` (
  `r_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `r_trener` int(11) unsigned NOT NULL,
  `r_kde` varchar(100) COLLATE cp1250_czech_cs NOT NULL,
  `r_datum` date NOT NULL,
  `r_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `r_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `r_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`r_id`),
  KEY `r_trener` (`r_trener`)
) ENGINE=MyISAM AUTO_INCREMENT=994 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `rozpis_item`
--
CREATE TABLE `rozpis_item` (
  `ri_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ri_id_rodic` int(11) unsigned NOT NULL,
  `ri_partner` int(11) unsigned NOT NULL DEFAULT '0',
  `ri_od` time NOT NULL,
  `ri_do` time NOT NULL,
  `ri_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`ri_id`)
) ENGINE=MyISAM AUTO_INCREMENT=7766 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `skupiny`
--
CREATE TABLE `skupiny` (
  `s_id` int(11) NOT NULL AUTO_INCREMENT,
  `s_name` varchar(50) COLLATE cp1250_czech_cs NOT NULL,
  `s_description` text COLLATE cp1250_czech_cs NOT NULL,
  `s_color_rgb` varchar(7) COLLATE cp1250_czech_cs NOT NULL,
  `s_color_text` varchar(20) COLLATE cp1250_czech_cs NOT NULL,
  PRIMARY KEY (`s_id`)
) ENGINE=MyISAM AUTO_INCREMENT=21 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `soutez`
--
CREATE TABLE `soutez` (
  `s_id` int(11) NOT NULL AUTO_INCREMENT,
  `s_idt` int(11) NOT NULL,
  `s_jmeno` text COLLATE cp1250_czech_cs NOT NULL,
  `s_prijmeni` text COLLATE cp1250_czech_cs NOT NULL,
  `s_1porotce` text COLLATE cp1250_czech_cs NOT NULL,
  `s_2porotce` text COLLATE cp1250_czech_cs NOT NULL,
  `s_3porotce` text COLLATE cp1250_czech_cs NOT NULL,
  `s_4porotce` text COLLATE cp1250_czech_cs NOT NULL,
  `s_5porotce` text COLLATE cp1250_czech_cs NOT NULL,
  `s_update` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`s_id`)
) ENGINE=MyISAM AUTO_INCREMENT=38 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `upozorneni`
--
CREATE TABLE `upozorneni` (
  `up_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `up_kdo` int(11) unsigned NOT NULL,
  `up_nadpis` text COLLATE cp1250_czech_cs NOT NULL,
  `up_text` text COLLATE cp1250_czech_cs NOT NULL,
  `up_barvy` int(11) NOT NULL DEFAULT '0',
  `up_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `up_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `up_timestamp_add` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`up_id`),
  KEY `up_kdo` (`up_kdo`),
  KEY `up_timestamp_add` (`up_timestamp_add`)
) ENGINE=MyISAM AUTO_INCREMENT=884 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `upozorneni_skupiny`
--
CREATE TABLE `upozorneni_skupiny` (
  `ups_id` int(11) NOT NULL AUTO_INCREMENT,
  `ups_id_rodic` int(11) NOT NULL,
  `ups_id_skupina` int(11) NOT NULL,
  `ups_color` varchar(255) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'white',
  `ups_popis` text COLLATE cp1250_czech_cs NOT NULL,
  PRIMARY KEY (`ups_id`)
) ENGINE=MyISAM AUTO_INCREMENT=525 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `users`
--
CREATE TABLE `users` (
  `u_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `u_login` char(20) COLLATE cp1250_czech_cs NOT NULL,
  `u_pass` char(40) COLLATE cp1250_czech_cs NOT NULL,
  `u_jmeno` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_prijmeni` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_pohlavi` enum('m','f') COLLATE cp1250_czech_cs NOT NULL,
  `u_email` varchar(50) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_telefon` char(17) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_narozeni` date NOT NULL DEFAULT '0000-00-00',
  `u_poznamky` text COLLATE cp1250_czech_cs,
  `u_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `u_level` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `u_group` int(11) NOT NULL,
  `u_skupina` int(11) NOT NULL DEFAULT '1',
  `u_dancer` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_ban` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_confirmed` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_temporary` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_system` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`u_id`),
  UNIQUE KEY `u_login` (`u_login`),
  KEY `u_jmeno` (`u_jmeno`),
  KEY `u_prijmeni` (`u_prijmeni`),
  KEY `u_narozeni` (`u_narozeni`)
) ENGINE=MyISAM AUTO_INCREMENT=758 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `users_platby`
--
CREATE TABLE `users_platby` (
  `up_id` int(11) NOT NULL AUTO_INCREMENT,
  `up_id_user` int(11) NOT NULL,
  `up_obdobi` varchar(255) CHARACTER SET utf8 COLLATE utf8_czech_ci NOT NULL,
  `up_castka` int(11) NOT NULL DEFAULT '0',
  `up_placeno` date NOT NULL,
  `up_plati_od` date NOT NULL,
  `up_plati_do` date NOT NULL,
  PRIMARY KEY (`up_id`)
) ENGINE=MyISAM AUTO_INCREMENT=302 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;

--
-- Table structure for table `users_skupiny`
--
CREATE TABLE `users_skupiny` (
  `us_id` int(11) NOT NULL AUTO_INCREMENT,
  `us_color` varchar(255) COLLATE utf8_czech_ci NOT NULL DEFAULT 'white',
  `us_platba_mesic` int(11) NOT NULL DEFAULT '0',
  `us_platba_ctvrtrok` int(11) NOT NULL DEFAULT '0',
  `us_platba_pulrok` int(11) NOT NULL DEFAULT '0',
  `us_popis` text COLLATE utf8_czech_ci NOT NULL,
  PRIMARY KEY (`us_id`)
) ENGINE=MyISAM AUTO_INCREMENT=14 DEFAULT CHARSET=utf8 COLLATE=utf8_czech_ci;

--
-- Table structure for table `video`
--
CREATE TABLE `video` (
  `v_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `v_name` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `v_date` date NOT NULL,
  `v_uri` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `v_text` text COLLATE cp1250_czech_cs NOT NULL,
  `v_playlist` tinyint(1) NOT NULL,
  `v_timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`v_id`)
) ENGINE=MyISAM AUTO_INCREMENT=126 DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs;
