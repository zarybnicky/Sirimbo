-- phpMyAdmin SQL Dump
-- version 3.3.7deb5build0.10.10.1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Jun 10, 2011 at 10:30 PM
-- Server version: 5.1.49
-- PHP Version: 5.3.3-1ubuntu9.5

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

--
-- Database: `olymp`
--

-- --------------------------------------------------------

--
-- Table structure for table `nabidka`
--

CREATE TABLE IF NOT EXISTS `nabidka` (
  `n_id` int(11) NOT NULL AUTO_INCREMENT,
  `n_trener` int(11) NOT NULL,
  `n_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `n_lock` tinyint(1) NOT NULL DEFAULT '0',
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

CREATE TABLE IF NOT EXISTS `nabidka_item` (
  `ni_id` int(11) NOT NULL AUTO_INCREMENT,
  `ni_id_rodic` int(11) NOT NULL,
  `ni_partner` int(11) NOT NULL,
  `ni_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `ni_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ni_id`),
  KEY `ni_id_rodice` (`ni_id_rodic`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `nabidka_item`
--


-- --------------------------------------------------------

--
-- Table structure for table `rozpis`
--

CREATE TABLE IF NOT EXISTS `rozpis` (
  `r_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `r_trener` int(11) NOT NULL,
  `r_kde` varchar(100) COLLATE cp1250_czech_cs NOT NULL,
  `r_datum` date NOT NULL,
  `r_od` time DEFAULT NULL,
  `r_do` time DEFAULT NULL,
  `r_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`r_id`),
  KEY `r_trener` (`r_trener`),
  KEY `r_kde` (`r_kde`),
  KEY `r_datum` (`r_datum`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `rozpis`
--


-- --------------------------------------------------------

--
-- Table structure for table `rozpis_item`
--

CREATE TABLE IF NOT EXISTS `rozpis_item` (
  `ri_id` int(11) NOT NULL AUTO_INCREMENT,
  `ri_id_rodic` int(11) NOT NULL,
  `ri_partner` int(11) NOT NULL,
  `ri_od` time NOT NULL,
  `ri_do` time NOT NULL,
  `ri_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ri_id`),
  KEY `ri_id_rodic` (`ri_id_rodic`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `rozpis_item`
--


-- --------------------------------------------------------

--
-- Table structure for table `upozorneni`
--

CREATE TABLE IF NOT EXISTS `upozorneni` (
  `up_id` int(11) NOT NULL AUTO_INCREMENT,
  `up_kdo` int(11) NOT NULL,
  `up_kdy` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `up_text` text COLLATE cp1250_czech_cs NOT NULL,
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

CREATE TABLE IF NOT EXISTS `users` (
  `u_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `u_login` char(20) COLLATE cp1250_czech_cs NOT NULL,
  `u_pass` char(32) COLLATE cp1250_czech_cs NOT NULL,
  `u_jmeno` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_prijmeni` varchar(40) COLLATE cp1250_czech_cs NOT NULL DEFAULT 'default',
  `u_email` varchar(50) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_telefon` char(13) COLLATE cp1250_czech_cs DEFAULT NULL,
  `u_poznamky` text COLLATE cp1250_czech_cs,
  `u_level` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `u_ban` tinyint(1) NOT NULL DEFAULT '0',
  `u_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`u_id`),
  KEY `u_jmeno` (`u_jmeno`),
  KEY `u_prijmeni` (`u_prijmeni`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `users`
--

