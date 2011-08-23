-- phpMyAdmin SQL Dump
-- version 3.3.10deb1
-- http://www.phpmyadmin.net
--
-- Počítač: localhost
-- Vygenerováno: Sobota 20. srpna 2011, 22:09
-- Verze MySQL: 5.1.54
-- Verze PHP: 5.3.5-1ubuntu7.2

SET FOREIGN_KEY_CHECKS=0;
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
-- Vytvoření: Sobota 20. srpna 2011, 22:08
-- Poslední změna: Sobota 20. srpna 2011, 22:08
--

CREATE TABLE IF NOT EXISTS `dokumenty` (
  `d_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `d_path` varchar(500) CHARACTER SET cp1250 COLLATE cp1250_czech_cs NOT NULL,
  `d_filename` varchar(500) NOT NULL,
  `d_kategorie` smallint(3) NOT NULL,
  `d_kdo` int(11) NOT NULL,
  `d_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`d_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

--
-- Vypisuji data pro tabulku `dokumenty`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `nabidka`
--
-- Vytvoření: Sobota 20. srpna 2011, 19:03
-- Poslední změna: Sobota 20. srpna 2011, 19:03
--

CREATE TABLE IF NOT EXISTS `nabidka` (
  `n_id` int(11) NOT NULL AUTO_INCREMENT,
  `n_trener` int(11) NOT NULL,
  `n_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `n_od` date NOT NULL,
  `n_do` date NOT NULL,
  `n_lock` tinyint(1) NOT NULL DEFAULT '0',
  `n_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`n_id`),
  KEY `n_trener` (`n_trener`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=2 ;

--
-- Vypisuji data pro tabulku `nabidka`
--

INSERT INTO `nabidka` (`n_id`, `n_trener`, `n_pocet_hod`, `n_od`, `n_do`, `n_lock`, `n_aktu`) VALUES
(1, 1, 5, '2011-09-01', '2011-09-01', 0, '0000-00-00 00:00:00');

-- --------------------------------------------------------

--
-- Struktura tabulky `nabidka_item`
--
-- Vytvoření: Pátek 19. srpna 2011, 12:20
-- Poslední změna: Sobota 20. srpna 2011, 14:21
-- Poslední kontrola: Pátek 19. srpna 2011, 12:20
--

CREATE TABLE IF NOT EXISTS `nabidka_item` (
  `ni_id` int(11) NOT NULL AUTO_INCREMENT,
  `ni_id_rodic` int(11) NOT NULL,
  `ni_partner` int(11) NOT NULL,
  `ni_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `ni_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ni_id`),
  UNIQUE KEY `ni_id_rodic` (`ni_id_rodic`,`ni_partner`),
  KEY `ni_id_rodice` (`ni_id_rodic`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Vypisuji data pro tabulku `nabidka_item`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `rozpis`
--
-- Vytvoření: Sobota 20. srpna 2011, 19:04
-- Poslední změna: Sobota 20. srpna 2011, 19:04
--

CREATE TABLE IF NOT EXISTS `rozpis` (
  `r_id` bigint(20) NOT NULL AUTO_INCREMENT,
  `r_trener` int(11) NOT NULL,
  `r_kde` varchar(100) COLLATE cp1250_czech_cs NOT NULL,
  `r_datum` date NOT NULL,
  `r_od` time NOT NULL,
  `r_do` time NOT NULL,
  `r_lock` tinyint(1) NOT NULL DEFAULT '0',
  `r_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`r_id`),
  KEY `r_trener` (`r_trener`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=2 ;

--
-- Vypisuji data pro tabulku `rozpis`
--

INSERT INTO `rozpis` (`r_id`, `r_trener`, `r_kde`, `r_datum`, `r_od`, `r_do`, `r_lock`, `r_aktu`) VALUES
(1, 3, 'SGO', '2011-08-16', '15:00:00', '20:00:00', 0, '0000-00-00 00:00:00');

-- --------------------------------------------------------

--
-- Struktura tabulky `rozpis_item`
--
-- Vytvoření: Sobota 20. srpna 2011, 08:07
-- Poslední změna: Sobota 20. srpna 2011, 14:19
--

CREATE TABLE IF NOT EXISTS `rozpis_item` (
  `ri_id` int(11) NOT NULL AUTO_INCREMENT,
  `ri_id_rodic` int(11) NOT NULL,
  `ri_partner` int(11) NOT NULL DEFAULT '0',
  `ri_od` time NOT NULL,
  `ri_do` time NOT NULL,
  `ri_lock` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ri_id`),
  UNIQUE KEY `ri_id_rodic_2` (`ri_id_rodic`,`ri_od`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=4 ;

--
-- Vypisuji data pro tabulku `rozpis_item`
--

INSERT INTO `rozpis_item` (`ri_id`, `ri_id_rodic`, `ri_partner`, `ri_od`, `ri_do`, `ri_lock`) VALUES
(1, 1, 0, '15:00:00', '16:00:00', 0),
(2, 1, 0, '16:00:00', '18:00:00', 0),
(3, 1, 0, '18:00:00', '20:00:00', 0);

-- --------------------------------------------------------

--
-- Struktura tabulky `upozorneni`
--
-- Vytvoření: Sobota 20. srpna 2011, 19:09
-- Poslední změna: Sobota 20. srpna 2011, 19:09
-- Poslední kontrola: Sobota 20. srpna 2011, 19:09
--

CREATE TABLE IF NOT EXISTS `upozorneni` (
  `up_id` int(11) NOT NULL AUTO_INCREMENT,
  `up_kdo` int(11) NOT NULL,
  `up_nadpis` text COLLATE cp1250_czech_cs NOT NULL,
  `up_text` text COLLATE cp1250_czech_cs NOT NULL,
  `up_lock` tinyint(1) NOT NULL DEFAULT '0',
  `up_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`up_id`),
  KEY `up_kdo` (`up_kdo`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Vypisuji data pro tabulku `upozorneni`
--


-- --------------------------------------------------------

--
-- Struktura tabulky `users`
--
-- Vytvoření: Pátek 19. srpna 2011, 12:15
-- Poslední změna: Sobota 20. srpna 2011, 22:06
-- Poslední kontrola: Sobota 20. srpna 2011, 22:06
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
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=6 ;

--
-- Vypisuji data pro tabulku `users`
--

INSERT INTO `users` (`u_id`, `u_login`, `u_pass`, `u_jmeno`, `u_prijmeni`, `u_email`, `u_telefon`, `u_poznamky`, `u_level`, `u_ban`, `u_lock`) VALUES
(1, 'admin', '21232f297a57a5a743894a0e4a801fc3', 'default', 'admini', '', '', '', 50, 0, 0),
(2, 'user', '6ad14ba9986e3615423dfca256d04e3f', 'user123', 'user123', '', '', '', 1, 0, 0),
(3, 'trener', 'f7ed5efb47e05188fa795865d64c7954', 'trener', 'trener', '', '', '', 6, 0, 0),
(4, 'editor', '5aee9dbd2a188839105073571bee1b1f', 'editor', 'editor', '', '', '', 5, 0, 0),
(5, 'superadmin', '17c4520f6cfd1ab53d8745e84681eb49', 'super', 'admin', NULL, NULL, NULL, 99, 0, 0);
SET FOREIGN_KEY_CHECKS=1;
COMMIT;
