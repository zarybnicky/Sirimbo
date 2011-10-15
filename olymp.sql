-- phpMyAdmin SQL Dump
-- version 3.3.10deb1
-- http://www.phpmyadmin.net
--
-- Počítač: localhost
-- Vygenerováno: Pondělí 10. října 2011, 21:41
-- Verze MySQL: 5.1.54
-- Verze PHP: 5.3.5-1ubuntu7.2

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT=0;
START TRANSACTION;


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Databáze: `olymp`
--

-- --------------------------------------------------------

--
-- Struktura tabulky `dokumenty`
--

DROP TABLE IF EXISTS `dokumenty`;
CREATE TABLE IF NOT EXISTS `dokumenty` (
  `d_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `d_path` varchar(50) CHARACTER SET cp1250 COLLATE cp1250_czech_cs NOT NULL,
  `d_name` varchar(80) NOT NULL,
  `d_filename` varchar(80) CHARACTER SET cp1250 COLLATE cp1250_czech_cs NOT NULL,
  `d_kategorie` smallint(3) NOT NULL,
  `d_kdo` int(11) unsigned NOT NULL,
  `d_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`d_id`),
  UNIQUE KEY `d_path` (`d_path`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=3 ;

--
-- Vypisuji data pro tabulku `dokumenty`
--

INSERT INTO `dokumenty` (`d_id`, `d_path`, `d_name`, `d_filename`, `d_kategorie`, `d_kdo`, `d_aktu`) VALUES
(1, 'upload/1315946099.php', 'test tabor', 'index.php', 3, 5, '2011-09-13 22:34:59');

-- --------------------------------------------------------

--
-- Struktura tabulky `nabidka`
--

DROP TABLE IF EXISTS `nabidka`;
CREATE TABLE IF NOT EXISTS `nabidka` (
  `n_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `n_trener` int(11) unsigned NOT NULL,
  `n_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `n_od` date NOT NULL,
  `n_do` date NOT NULL,
  `n_visible` tinyint(1) NOT NULL DEFAULT '0',
  `n_lock` tinyint(1) NOT NULL DEFAULT '0',
  `n_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`n_id`),
  KEY `n_trener` (`n_trener`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=3 ;

--
-- Vypisuji data pro tabulku `nabidka`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `nabidka_item`
--

DROP TABLE IF EXISTS `nabidka_item`;
CREATE TABLE IF NOT EXISTS `nabidka_item` (
  `ni_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ni_id_rodic` int(11) unsigned NOT NULL,
  `ni_partner` int(11) unsigned NOT NULL,
  `ni_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `ni_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ni_id`),
  UNIQUE KEY `ni_id_rodic` (`ni_id_rodic`,`ni_partner`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=5 ;

--
-- Vypisuji data pro tabulku `nabidka_item`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `pary`
--

DROP TABLE IF EXISTS `pary`;
CREATE TABLE IF NOT EXISTS `pary` (
  `p_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `p_id_partner` int(11) unsigned NOT NULL,
  `p_id_partnerka` int(11) unsigned NOT NULL,
  `p_archiv` tinyint(1) NOT NULL DEFAULT '0',
  `p_aktu_vytvoreno` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `p_aktu_archivovano` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`p_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=16 ;

--
-- Vypisuji data pro tabulku `pary`
--

INSERT INTO `pary` (`p_id`, `p_id_partner`, `p_id_partnerka`, `p_archiv`, `p_aktu_vytvoreno`, `p_aktu_archivovano`) VALUES
(1, 5, 11, 1, '2011-10-09 22:47:38', '2011-10-09 22:47:43'),
(2, 11, 0, 1, '2011-10-09 22:47:43', '2011-10-09 22:47:43'),
(3, 5, 0, 1, '2011-10-09 22:47:43', '2011-10-09 22:47:47'),
(4, 5, 11, 1, '2011-10-09 22:47:47', '2011-10-09 22:49:36'),
(5, 5, 0, 1, '2011-10-09 22:49:36', '2011-10-09 22:49:41'),
(6, 11, 0, 1, '2011-10-09 22:49:36', '2011-10-09 22:49:44'),
(7, 5, 0, 1, '2011-10-09 22:49:41', '2011-10-09 22:51:01'),
(8, 11, 0, 1, '2011-10-09 22:49:44', '2011-10-09 22:51:20'),
(9, 5, 0, 1, '2011-10-09 22:51:01', '2011-10-09 22:51:40'),
(10, 11, 0, 1, '2011-10-09 22:51:20', '2011-10-09 22:51:44'),
(11, 5, 0, 1, '2011-10-09 22:51:40', '2011-10-09 22:51:47'),
(12, 11, 0, 1, '2011-10-09 22:51:44', '2011-10-09 22:51:50'),
(13, 5, 0, 1, '2011-10-09 22:51:47', '2011-10-09 22:51:55'),
(14, 11, 0, 1, '2011-10-09 22:51:50', '2011-10-09 22:51:55'),
(15, 5, 11, 0, '2011-10-09 22:51:55', '0000-00-00 00:00:00');

-- --------------------------------------------------------

--
-- Struktura tabulky `rozpis`
--

DROP TABLE IF EXISTS `rozpis`;
CREATE TABLE IF NOT EXISTS `rozpis` (
  `r_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `r_trener` int(11) unsigned NOT NULL,
  `r_kde` varchar(100) COLLATE cp1250_czech_cs NOT NULL,
  `r_datum` date NOT NULL,
  `r_visible` tinyint(1) NOT NULL DEFAULT '0',
  `r_lock` tinyint(1) NOT NULL DEFAULT '0',
  `r_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`r_id`),
  KEY `r_trener` (`r_trener`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=5 ;

--
-- Vypisuji data pro tabulku `rozpis`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `rozpis_item`
--

DROP TABLE IF EXISTS `rozpis_item`;
CREATE TABLE IF NOT EXISTS `rozpis_item` (
  `ri_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ri_id_rodic` int(11) unsigned NOT NULL,
  `ri_partner` int(11) unsigned NOT NULL DEFAULT '0',
  `ri_od` time NOT NULL,
  `ri_do` time NOT NULL,
  `ri_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ri_id`),
  UNIQUE KEY `ri_id_rodic_2` (`ri_id_rodic`,`ri_od`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=12 ;

--
-- Vypisuji data pro tabulku `rozpis_item`
--

INSERT INTO `rozpis_item` (`ri_id`, `ri_id_rodic`, `ri_partner`, `ri_od`, `ri_do`, `ri_lock`) VALUES
(9, 3, 0, '14:00:00', '15:00:00', 0),
(7, 3, 0, '12:30:00', '13:15:00', 0),
(10, 3, 0, '15:00:00', '15:45:00', 0),
(11, 3, 0, '15:45:00', '16:30:00', 0);

-- --------------------------------------------------------

--
-- Struktura tabulky `tas`
--

DROP TABLE IF EXISTS `tas`;
CREATE TABLE IF NOT EXISTS `tas` (
  `t_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `t_jmeno` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `t_kde` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `t_info` text COLLATE cp1250_czech_cs NOT NULL,
  `t_od` date NOT NULL,
  `t_do` date NOT NULL,
  `t_kapacita` int(11) NOT NULL DEFAULT '0',
  `t_dokumenty` blob NOT NULL,
  `t_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `t_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`t_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=3 ;

--
-- Vypisuji data pro tabulku `tas`
--

INSERT INTO `tas` (`t_id`, `t_jmeno`, `t_kde`, `t_info`, `t_od`, `t_do`, `t_kapacita`, `t_dokumenty`, `t_aktu`, `t_lock`) VALUES
(1, 'Test', 'Test place', 'spfben fdv=infmd\r\nsveaspfviujnd\r\nevnfjuf\r\n<b>test</b>', '2011-09-05', '2011-09-28', 50, 0x613a313a7b693a303b733a313a2231223b7d, '2011-09-17 17:16:33', 50);

-- --------------------------------------------------------

--
-- Struktura tabulky `tas_item`
--

DROP TABLE IF EXISTS `tas_item`;
CREATE TABLE IF NOT EXISTS `tas_item` (
  `ti_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ti_id_rodic` int(11) unsigned NOT NULL,
  `ti_user` int(11) unsigned DEFAULT NULL,
  `ti_jmeno` varchar(40) COLLATE cp1250_czech_cs NOT NULL,
  `ti_prijmeni` varchar(40) COLLATE cp1250_czech_cs NOT NULL,
  `ti_rok_narozeni` smallint(4) NOT NULL,
  PRIMARY KEY (`ti_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=10 ;

--
-- Vypisuji data pro tabulku `tas_item`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `upozorneni`
--

DROP TABLE IF EXISTS `upozorneni`;
CREATE TABLE IF NOT EXISTS `upozorneni` (
  `up_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `up_kdo` int(11) unsigned NOT NULL,
  `up_nadpis` text COLLATE cp1250_czech_cs NOT NULL,
  `up_text` text COLLATE cp1250_czech_cs NOT NULL,
  `up_barvy` int(11) NOT NULL DEFAULT '0',
  `up_lock` tinyint(1) NOT NULL DEFAULT '0',
  `up_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`up_id`),
  KEY `up_kdo` (`up_kdo`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=5 ;

--
-- Vypisuji data pro tabulku `upozorneni`
--

INSERT INTO `upozorneni` (`up_id`, `up_kdo`, `up_nadpis`, `up_text`, `up_barvy`, `up_lock`, `up_aktu`) VALUES
(4, 5, 'Test', 'dfvpoxv', 7, 0, '2011-10-06 17:31:34');

-- --------------------------------------------------------

--
-- Struktura tabulky `users`
--

DROP TABLE IF EXISTS `users`;
CREATE TABLE IF NOT EXISTS `users` (
  `u_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `u_login` char(20) COLLATE cp1250_czech_cs NOT NULL,
  `u_pass` char(40) COLLATE cp1250_czech_cs NOT NULL,
  `u_jmeno` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_prijmeni` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_pohlavi` enum('m','f') COLLATE cp1250_czech_cs NOT NULL,
  `u_email` varchar(50) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_telefon` char(13) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_poznamky` text COLLATE cp1250_czech_cs,
  `u_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `u_level` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `u_ban` tinyint(1) NOT NULL DEFAULT '0',
  `u_lock` tinyint(1) NOT NULL DEFAULT '0',
  `u_confirmed` tinyint(1) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`u_id`),
  UNIQUE KEY `u_login` (`u_login`),
  KEY `u_jmeno` (`u_jmeno`),
  KEY `u_prijmeni` (`u_prijmeni`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=12 ;

--
-- Vypisuji data pro tabulku `users`
--

INSERT INTO `users` (`u_id`, `u_login`, `u_pass`, `u_jmeno`, `u_prijmeni`, `u_pohlavi`, `u_email`, `u_telefon`, `u_poznamky`, `u_aktu`, `u_level`, `u_ban`, `u_lock`, `u_confirmed`) VALUES
(11, 'admin', 'b24d1101fb1a43744e132c0241fc2c011549fbf9', 'default', 'admin', 'f', '', '', '', '2011-10-09 18:32:50', 50, 0, 0, 1),
(5, 'superadmin', '9947a7bc1549a54e7299fe9a3975c8655430ade0', 'super', 'admin', 'm', '', '', '', '2011-10-09 17:39:57', 50, 0, 0, 1);
COMMIT;
