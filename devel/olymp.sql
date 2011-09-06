-- phpMyAdmin SQL Dump
-- version 3.3.10deb1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Sep 04, 2011 at 03:57 PM
-- Server version: 5.1.54
-- PHP Version: 5.3.5-1ubuntu7.2

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT=0;
START TRANSACTION;

SET NAMES utf8;

--
-- Database: `olymp`
--

-- --------------------------------------------------------

--
-- Table structure for table `dokumenty`
--
-- Creation: Sep 04, 2011 at 03:56 PM
-- Last update: Sep 04, 2011 at 03:56 PM
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
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

--
-- Dumping data for table `dokumenty`
--


-- --------------------------------------------------------

--
-- Table structure for table `nabidka`
--
-- Creation: Sep 04, 2011 at 03:56 PM
-- Last update: Sep 04, 2011 at 03:56 PM
--

DROP TABLE IF EXISTS `nabidka`;
CREATE TABLE IF NOT EXISTS `nabidka` (
  `n_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `n_trener` int(11) unsigned NOT NULL,
  `n_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `n_od` date NOT NULL,
  `n_do` date NOT NULL,
  `n_lock` tinyint(1) NOT NULL DEFAULT '0',
  `n_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`n_id`),
  KEY `n_trener` (`n_trener`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `nabidka`
--


-- --------------------------------------------------------

--
-- Table structure for table `nabidka_item`
--
-- Creation: Sep 04, 2011 at 03:56 PM
-- Last update: Sep 04, 2011 at 03:56 PM
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
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `nabidka_item`
--


-- --------------------------------------------------------

--
-- Table structure for table `rozpis`
--
-- Creation: Sep 04, 2011 at 03:56 PM
-- Last update: Sep 04, 2011 at 03:56 PM
--

DROP TABLE IF EXISTS `rozpis`;
CREATE TABLE IF NOT EXISTS `rozpis` (
  `r_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `r_trener` int(11) unsigned NOT NULL,
  `r_kde` varchar(100) COLLATE cp1250_czech_cs NOT NULL,
  `r_datum` date NOT NULL,
  `r_lock` tinyint(1) NOT NULL DEFAULT '0',
  `r_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`r_id`),
  KEY `r_trener` (`r_trener`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `rozpis`
--


-- --------------------------------------------------------

--
-- Table structure for table `rozpis_item`
--
-- Creation: Sep 04, 2011 at 03:56 PM
-- Last update: Sep 04, 2011 at 03:56 PM
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
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `rozpis_item`
--


-- --------------------------------------------------------

--
-- Table structure for table `tas`
--
-- Creation: Sep 04, 2011 at 03:52 PM
-- Last update: Sep 04, 2011 at 03:52 PM
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
  `t_dokumenty` text COLLATE cp1250_czech_cs NOT NULL,
  `t_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `t_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`t_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `tas`
--


-- --------------------------------------------------------

--
-- Table structure for table `tas_item`
--
-- Creation: Sep 04, 2011 at 03:55 PM
-- Last update: Sep 04, 2011 at 03:55 PM
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
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `tas_item`
--


-- --------------------------------------------------------

--
-- Table structure for table `upozorneni`
--
-- Creation: Sep 04, 2011 at 03:56 PM
-- Last update: Sep 04, 2011 at 03:56 PM
--

DROP TABLE IF EXISTS `upozorneni`;
CREATE TABLE IF NOT EXISTS `upozorneni` (
  `up_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `up_kdo` int(11) unsigned NOT NULL,
  `up_nadpis` text COLLATE cp1250_czech_cs NOT NULL,
  `up_text` text COLLATE cp1250_czech_cs NOT NULL,
  `up_lock` tinyint(1) NOT NULL DEFAULT '0',
  `up_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`up_id`),
  KEY `up_kdo` (`up_kdo`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `upozorneni`
--


-- --------------------------------------------------------

--
-- Table structure for table `users`
--
-- Creation: Aug 19, 2011 at 12:15 PM
-- Last update: Aug 24, 2011 at 04:53 PM
-- Last check: Aug 20, 2011 at 10:06 PM
--

DROP TABLE IF EXISTS `users`;
CREATE TABLE IF NOT EXISTS `users` (
  `u_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `u_login` char(20) COLLATE cp1250_czech_cs NOT NULL,
  `u_pass` char(32) COLLATE cp1250_czech_cs NOT NULL,
  `u_jmeno` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_prijmeni` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_email` varchar(50) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_telefon` char(13) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_poznamky` text COLLATE cp1250_czech_cs,
  `u_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `u_level` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `u_ban` tinyint(1) NOT NULL DEFAULT '0',
  `u_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`u_id`),
  KEY `u_jmeno` (`u_jmeno`),
  KEY `u_prijmeni` (`u_prijmeni`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=7 ;

--
-- Dumping data for table `users`
--

INSERT INTO `users` (`u_id`, `u_login`, `u_pass`, `u_jmeno`, `u_prijmeni`, `u_email`, `u_telefon`, `u_poznamky`, `u_level`, `u_ban`, `u_lock`) VALUES
(1, 'admin', '21232f297a57a5a743894a0e4a801fc3', 'default', 'admini', '', '', '', 50, 0, 0),
(2, 'user', '6ad14ba9986e3615423dfca256d04e3f', 'user123', 'user123', '', '', '', 1, 1, 0),
(3, 'trener', 'f7ed5efb47e05188fa795865d64c7954', 'trener', 'trener', '', '', '', 6, 0, 0),
(4, 'editor', '5aee9dbd2a188839105073571bee1b1f', 'editor', 'editor', '', '', '', 5, 0, 0),
(5, 'superadmin', '17c4520f6cfd1ab53d8745e84681eb49', 'super', 'admin', '', '', '', 99, 0, 0);
COMMIT;
