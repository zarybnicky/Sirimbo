-- phpMyAdmin SQL Dump
-- version 3.4.5deb1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Dec 09, 2011 at 03:33 PM
-- Server version: 5.1.58
-- PHP Version: 5.3.6-13ubuntu3.2

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT=0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `olymp`
--

-- --------------------------------------------------------

--
-- Table structure for table `akce`
--

DROP TABLE IF EXISTS `akce`;
CREATE TABLE IF NOT EXISTS `akce` (
  `a_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `a_jmeno` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `a_kde` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `a_info` text COLLATE cp1250_czech_cs NOT NULL,
  `a_od` date NOT NULL,
  `a_do` date NOT NULL,
  `a_kapacita` int(11) NOT NULL DEFAULT '0',
  `a_dokumenty` blob NOT NULL,
  `a_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `a_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`a_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `akce_item`
--

DROP TABLE IF EXISTS `akce_item`;
CREATE TABLE IF NOT EXISTS `akce_item` (
  `ai_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ai_id_rodic` int(11) unsigned NOT NULL,
  `ai_user` int(11) unsigned DEFAULT NULL,
  `ai_jmeno` varchar(40) COLLATE cp1250_czech_cs NOT NULL,
  `ai_prijmeni` varchar(40) COLLATE cp1250_czech_cs NOT NULL,
  `ai_rok_narozeni` smallint(4) NOT NULL,
  PRIMARY KEY (`ai_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `aktuality`
--

DROP TABLE IF EXISTS `aktuality`;
CREATE TABLE IF NOT EXISTS `aktuality` (
  `at_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `at_kdo` int(11) unsigned NOT NULL,
  `at_kat` enum('1','2') COLLATE cp1250_czech_cs NOT NULL,
  `at_jmeno` varchar(255) COLLATE cp1250_czech_cs NOT NULL,
  `at_text` text COLLATE cp1250_czech_cs NOT NULL,
  `at_preview` varchar(200) COLLATE cp1250_czech_cs NOT NULL,
  `at_foto` blob NOT NULL,
  `at_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`at_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `ankety`
--

DROP TABLE IF EXISTS `ankety`;
CREATE TABLE IF NOT EXISTS `ankety` (
  `ak_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ak_kdo` int(11) unsigned NOT NULL,
  `ak_jmeno` text COLLATE cp1250_czech_cs NOT NULL,
  `ak_text` text COLLATE cp1250_czech_cs NOT NULL,
  `ak_pristup` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '1',
  `ak_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `ak_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`ak_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `ankety_ip`
--

DROP TABLE IF EXISTS `ankety_ip`;
CREATE TABLE IF NOT EXISTS `ankety_ip` (
  `akp_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `akp_id_rodic` int(11) unsigned NOT NULL,
  `akp_ip` int(11) unsigned NOT NULL,
  `akp_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`akp_id`),
  UNIQUE KEY `akp_id_rodic` (`akp_id_rodic`,`akp_ip`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `ankety_item`
--

DROP TABLE IF EXISTS `ankety_item`;
CREATE TABLE IF NOT EXISTS `ankety_item` (
  `aki_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `aki_id_rodic` int(11) unsigned NOT NULL,
  `aki_text` text COLLATE cp1250_czech_cs NOT NULL,
  `aki_pocet` mediumint(9) NOT NULL,
  PRIMARY KEY (`aki_id`),
  KEY `aki_pocet` (`aki_pocet`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `dokumenty`
--

DROP TABLE IF EXISTS `dokumenty`;
CREATE TABLE IF NOT EXISTS `dokumenty` (
  `d_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `d_path` varchar(50) COLLATE cp1250_czech_cs NOT NULL,
  `d_name` varchar(80) COLLATE cp1250_czech_cs NOT NULL,
  `d_filename` varchar(80) COLLATE cp1250_czech_cs NOT NULL,
  `d_kategorie` smallint(3) NOT NULL,
  `d_kdo` int(11) unsigned NOT NULL,
  `d_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`d_id`),
  UNIQUE KEY `d_path` (`d_path`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `galerie_dir`
--

DROP TABLE IF EXISTS `galerie_dir`;
CREATE TABLE IF NOT EXISTS `galerie_dir` (
  `gd_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `gd_id_rodic` int(11) unsigned NOT NULL,
  `gd_name` text COLLATE cp1250_czech_cs NOT NULL,
  `gd_level` smallint(6) NOT NULL DEFAULT '1',
  PRIMARY KEY (`gd_id`),
  KEY `gd_id_rodic` (`gd_id_rodic`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

--
-- Dumping data for table `galerie_dir`
--

INSERT INTO `galerie_dir` (`gd_id`, `gd_id_rodic`, `gd_name`, `gd_level`) VALUES
(0, 0, 'Hlavní', 1);

-- --------------------------------------------------------

--
-- Table structure for table `galerie_foto`
--

DROP TABLE IF EXISTS `galerie_foto`;
CREATE TABLE IF NOT EXISTS `galerie_foto` (
  `gf_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `gf_id_rodic` int(11) unsigned NOT NULL,
  `gf_name` text COLLATE cp1250_czech_cs NOT NULL,
  `gf_path` text COLLATE cp1250_czech_cs NOT NULL,
  `gf_kdo` int(11) unsigned NOT NULL,
  `gf_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`gf_id`),
  KEY `gf_id_rodic` (`gf_id_rodic`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `inzerce`
--

DROP TABLE IF EXISTS `inzerce`;
CREATE TABLE IF NOT EXISTS `inzerce` (
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
  `i_confirmed` enum('0','1') COLLATE cp1250_czech_cs NOT NULL,
  `i_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL,
  `i_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`i_id`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `nabidka`
--

DROP TABLE IF EXISTS `nabidka`;
CREATE TABLE IF NOT EXISTS `nabidka` (
  `n_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `n_trener` int(11) unsigned NOT NULL,
  `n_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `n_od` date NOT NULL,
  `n_do` date NOT NULL,
  `n_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `n_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `n_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`n_id`),
  KEY `n_trener` (`n_trener`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `nabidka_item`
--

DROP TABLE IF EXISTS `nabidka_item`;
CREATE TABLE IF NOT EXISTS `nabidka_item` (
  `ni_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ni_id_rodic` int(11) unsigned NOT NULL,
  `ni_partner` int(11) unsigned NOT NULL,
  `ni_pocet_hod` smallint(3) NOT NULL DEFAULT '1',
  `ni_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`ni_id`),
  UNIQUE KEY `ni_id_rodic` (`ni_id_rodic`,`ni_partner`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `novinky`
--

DROP TABLE IF EXISTS `novinky`;
CREATE TABLE IF NOT EXISTS `novinky` (
  `no_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `no_text` text COLLATE cp1250_czech_cs NOT NULL,
  `no_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`no_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `pary`
--

DROP TABLE IF EXISTS `pary`;
CREATE TABLE IF NOT EXISTS `pary` (
  `p_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `p_id_partner` int(11) unsigned NOT NULL,
  `p_id_partnerka` int(11) unsigned NOT NULL,
  `p_stt_trida` enum('Z','H','D','C','B','A','M') COLLATE cp1250_czech_cs NOT NULL,
  `p_stt_body` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `p_stt_finale` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `p_lat_trida` enum('Z','H','D','C','B','A','M') COLLATE cp1250_czech_cs NOT NULL,
  `p_lat_body` tinyint(3) unsigned NOT NULL DEFAULT '0',
  `p_lat_finale` tinyint(1) unsigned NOT NULL DEFAULT '0',
  `p_hodnoceni` smallint(5) unsigned NOT NULL,
  `p_archiv` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `p_aktu_vytvoreno` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `p_aktu_archivovano` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  PRIMARY KEY (`p_id`),
  KEY `p_hodnoceni` (`p_hodnoceni`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `pary_navrh`
--

DROP TABLE IF EXISTS `pary_navrh`;
CREATE TABLE IF NOT EXISTS `pary_navrh` (
  `pn_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `pn_navrhl` int(11) unsigned NOT NULL,
  `pn_partner` int(11) unsigned NOT NULL,
  `pn_partnerka` int(11) unsigned NOT NULL,
  PRIMARY KEY (`pn_id`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `rozpis`
--

DROP TABLE IF EXISTS `rozpis`;
CREATE TABLE IF NOT EXISTS `rozpis` (
  `r_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `r_trener` int(11) unsigned NOT NULL,
  `r_kde` varchar(100) COLLATE cp1250_czech_cs NOT NULL,
  `r_datum` date NOT NULL,
  `r_visible` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `r_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `r_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`r_id`),
  KEY `r_trener` (`r_trener`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `rozpis_item`
--

DROP TABLE IF EXISTS `rozpis_item`;
CREATE TABLE IF NOT EXISTS `rozpis_item` (
  `ri_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `ri_id_rodic` int(11) unsigned NOT NULL,
  `ri_partner` int(11) unsigned NOT NULL DEFAULT '0',
  `ri_od` time NOT NULL,
  `ri_do` time NOT NULL,
  `ri_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`ri_id`),
  UNIQUE KEY `ri_id_rodic_2` (`ri_id_rodic`,`ri_od`)
) ENGINE=MyISAM DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Table structure for table `upozorneni`
--

DROP TABLE IF EXISTS `upozorneni`;
CREATE TABLE IF NOT EXISTS `upozorneni` (
  `up_id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `up_kdo` int(11) unsigned NOT NULL,
  `up_nadpis` text COLLATE cp1250_czech_cs NOT NULL,
  `up_text` text COLLATE cp1250_czech_cs NOT NULL,
  `up_barvy` int(11) NOT NULL DEFAULT '0',
  `up_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `up_aktu` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`up_id`),
  KEY `up_kdo` (`up_kdo`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=335 ;

--
-- Dumping data for table `upozorneni`
--

INSERT INTO `upozorneni` (`up_id`, `up_kdo`, `up_nadpis`, `up_text`, `up_barvy`, `up_lock`, `up_aktu`) VALUES
(1, 1, 'Ze starých stránek', 'Lekce s Pavlou Landsfeldovou se budou platit na prektyse!', 7, '0', '2009-09-23 22:00:00'),
(2, 1, 'Ze starých stránek', '<b>Taneční soutěž Memoriál Dr. Miroslava Hýži</b><br><br>  Náš klub bude dne 17.listopadu (úterý - státní svátek) v Regionálním centu pořádat taneční soutěž, na které jsou vypsány kategorie pro všechny páry našeho klubu. Většina z členů TK OLYMP již byla automaticky přihlášena. Na klubové soutěži by měly startovat VŠECHNY páry, které v našem klubu trénují. <br><br> Dále bychom chtěli požádat rodiče, kteří mají nějaké možnosti, aby se pokusili sehnat nějaké ceny do soutěže nebo finanční sponzorský dar.', 7, '0', '2009-09-24 22:00:00'),
(3, 1, 'Ze starých stránek', 'V pátek 2.9. se nebudou konat pravidelné tréninky s Pavlem Johnem na ZŠ Holečkova, v sekci rozpisy tréninků si můžete rezervovat lekce od 16 do 19 hodin na Slovanském gymnáziu. Zároveň byla výjimečně zrušena individuální lekce Jiřího Anderleho s Tomášem Krainou z důvodu obsazení všech sálů.', 7, '0', '2009-09-28 22:00:00'),
(4, 1, 'Ze starých stránek', 'Nejčastější účastníci prektysů budou každý měsíc odmeněni individuální lekci s Tomášem Krainou nebo Miroslavem Hýžou, kterou uhradí klub.', 7, '0', '2009-10-01 22:00:00'),
(5, 1, 'Ze starých stránek', 'Tréninky se Zdeňkem Landsfeldem se budou platit ve čtvrtek na prektyse.', 7, '0', '2009-10-06 22:00:00'),
(6, 1, 'Ze starých stránek', 'Příští týden od středy až do pátku (28.-30.10.) se z důvodu prázdnin nebudou konat žádné společné lekce, practise ani pilates. V pátek je z důvodu deratizace zákaz vstupu do baletního sálu.', 7, '0', '2009-10-19 22:00:00'),
(7, 1, 'Ze starých stránek', '<b>Příspěvky ČSTS na rok 2010</b><br><br> budu vybírat v těchto termínech: 2.11. a 9.11. před společnou lekcí s Kamilou Limberkovou na baletním sále, 3.11. a 10.11. před společnou lekcí s Martinem Odstrčilem na Slovanském gymnáziu. <br><br>Dospělí: 1050,-Kč, děti do 12-ti let: 900,-Kč, porotce I.: 1000,- Kč (noví členové + 50,- Kč)<br><br>                         Jendrulková Hana-pokladník ', 7, '0', '2009-10-20 22:00:00'),
(8, 1, 'Ze starých stránek', 'Změna času vedené hodiny s Martinem Odstrčilem v úterý.', 7, '0', '2009-10-24 22:00:00'),
(9, 1, 'Ze starých stránek', 'V podzimních prázdninách (28.10 - 1.11.) se ruší všechny pravidelné tréninky. Do sekce rozpis tréninků budou přidány prázdné rozpisy s klubovými trenéry, kde si budete moci zarezervovat lekce. ', 7, '0', '2009-10-24 22:00:00'),
(10, 1, 'Ze starých stránek', '<b>Zajemci o lekce s Tomášem Krainou na 31.10.</b><br><br>  Na stránky byl vložen rozpis a nabídka tréninků s Tomášem. V nabídce si zarezervujte počet lekcí (nejvýše dvě). Podle této nabídky bude ve čtvrtek sestaven rozpis, který je zatím prázdný a nejde se do něj zapsat. Pokud máte nějaké časové požadavky, napište mi je na 737 545 525 nebo hyzam@tkolymp.cz nebo ICQ 209070245. Takové opatření se dělá proto, aby vyšlo na co možná největší počet zájemců o lekce. Pokud jich nebude více než pět, doustanou lekce ti, kteří si v nabídce zarezervují lekce 2. <br><br> Děkuji za pochopení, Miroslav Hýža', 7, '0', '2009-10-24 22:00:00'),
(11, 1, 'Ze starých stránek', 'Tréninky s Jardou Kučerou v pátek se ruší, přijede příští týden.', 7, '0', '2009-10-28 23:00:00'),
(12, 1, 'Ze starých stránek', '<b>Funkčnost webu byla po technické závadě, způsobené provozovatelem serveru, obnovena. </b>', 7, '0', '2009-11-17 23:00:00'),
(13, 1, 'Ze starých stránek', 'Pavel John v pátek z důvodu nemoci neučí.', 7, '0', '2009-11-17 23:00:00'),
(14, 1, 'Ze starých stránek', 'Děkujeme všem rodičům a tanečníkům za pomoc při pořádání taneční soutěže. ', 7, '0', '2009-11-17 23:00:00'),
(15, 1, 'Ze starých stránek', 'Páteční tréninky s Tomášem Krainou jsou na ZŠ Holečkova.', 7, '0', '2009-11-18 23:00:00'),
(16, 1, 'Ze starých stránek', 'Za plnou účast na prektysech v měsíci listoapdu budou <b>odměněny páry Homola-Uvízlová, Pecha-Sirotinová, Švarc-Vymětalová a Pastrňák-Benýšková</b> jednou lekcí zdarma s Tomášem Krainou, která se uskuteční pravděpodobně v sobotu 5.12.. Přesné časy, se objeví v nejbližších dnech.', 7, '0', '2009-11-26 23:00:00'),
(17, 1, 'Ze starých stránek', '<b>Odměny za prektysy a pohybovky</b><br><br> Od příštího měsíce se budou odměny rozdávat nejen za účast na prektysech ale také dohromady za účast na pohybovkách!', 7, '0', '2009-11-26 23:00:00'),
(18, 1, 'Ze starých stránek', '<b>!!!!  Změna úterních rozpisů  !!!!</b><br><br>Byly pozměněny úterní rozpisy tréninků s Martinem Odstrčilem a Miroslavem Hýžou. Důvodem jsou lekce s Jardou Kučerou, které se musely časově skloubit se standardními lekcemi. Děkujeme všem za pochopení.', 7, '0', '2009-11-26 23:00:00'),
(19, 1, 'Ze starých stránek', '<b>Mikulášská besídka</b><br><br>  v pátek 4.12. se na ZŠ Holečkova ve velké tělocvičně uskuteční Mikulášská besídka. Program začíná v 17:00, během programu se v tanečních ukázkách představí všichni členové TK Olymp. Sraz tanečníků je v 16:40 ve velké tělocvičně. Mezi pozvanými hosty jsou i čert a mikuláš. Pozvěte si rodiče, prarodiče a přátele. Vstup je zdarma.  ', 7, '0', '2009-11-28 23:00:00'),
(20, 1, 'Ze starých stránek', 'Páteční tréninky s Tomášem Krainou, Pavlem Johnem a Luckou Benýškovou se ruší. Důvodem je Mikulášská besídka.', 7, '0', '2009-11-28 23:00:00'),
(21, 1, 'Ze starých stránek', 'Na homepage můžete najít odkaz na výsledky a krátké shrnutí výkonů našich tanečníků na letošním Memoriálu. <br> <br> Přímý odkaz zde: <br> http://tkolymp.cz/AKCE/06_MEMORI%C3%81L%202009/AkceInfo.aspx', 7, '0', '2009-11-28 23:00:00'),
(22, 1, 'Ze starých stránek', 'Všichni, kdo mají zájem o atletickou průpravu, přizpůsobenou tanečnímu sportu, ozvěte se Rudovi Stupkovi na tel: 607 970 295. Tréninky by měly probíhat každou středu odpoledne.', 7, '0', '2009-12-04 23:00:00'),
(23, 1, 'Ze starých stránek', '<b>ZMĚNA PÁTEČNÍCH TRÉNINKŮ</b><br><br>  Tréninky s Tomášem Krainou jsou na pátek 11.prosince přesunuty na Slovanské gymnázium. Treninky s Pavlem Johnem byly zrušeny. Nově jsou přidány lekce s Miroslavem Hýžou.', 7, '0', '2009-12-05 23:00:00'),
(24, 1, 'Ze starých stránek', 'Úterní lekce s Martinem Odstrčilem byly změněny.', 7, '0', '2009-12-06 23:00:00'),
(25, 1, 'Ze starých stránek', '<b>Vánoční soustředění</b><br><br> Ve dnech 19. a 20. prosince se uskuteční Vánoční soustředění TK Olymp Olomouc. Podrobné informace a podmínky účasti Vám budou sděleny v nejbližších dnech prostřednictvím trenérů na společných lekcích, na prektyse a zde na webových stránkách.', 7, '0', '2009-12-07 23:00:00'),
(26, 1, 'Ze starých stránek', '<b>!!!DŮLEŽITÉ!!!</b><br><br> - Členské příspěvky na I.pololetí r. 2010 (ve stejné výši) je nutno zaplatit do 31.ledna 2010.<br> Hotově: p. Jendrulková <br>    Č. účtu: 1806875329-0800', 7, '0', '2009-12-08 23:00:00'),
(27, 1, 'Ze starých stránek', '<b>Vánoční soustředění</b> (19.-20.12.)<br><br>V sekci Tábory a soustředění se můžete hlásit na Vánoční soustředění. Cena pro děti je 200 Kč/pár a obsahuje 2 vedené a dvě individuální lekce, cena pro juniory a dospělé je 400 Kč/pár a zahrnuje 4 individuální a 4 společné lekce. V neděli od 19 hodin bude soustředění zakončeno prektysem. <b>SOUSTŘEDĚNÍ SE TÝKÁ VŠECH PÁRŮ TK OLYMP</b><br><br>Trenéři: Tomáš Kraina, Miroslav Hýža, Pavel John, Tereza Jendrulková, Lucie Benýšková<br><br>Platby za soustředění se budou vybírat v pondělí a úterý na trénincích. <br><b>Přihlašovat na soustředění se můžete do  úterý!</b><br><br>Případné požadavky na termíny individuálních lekcí zasílejte na hyzam@tkolymp.cz', 7, '0', '2009-12-09 23:00:00'),
(28, 1, 'Ze starých stránek', 'Na přání pořadatelů soutěže Kometa Cup 2009 (19.12.) byly páry TK Olymp přihlášeny. Pokud by nakonec v této soutěži nestartovaly, sdělte mi to a já vás těsně před soutěží odhlásím. Tato vzájemná spolupráce funguje vrámci přátelských vztahů s klubem DSP Kometa Brno, stejným způsobem pomohl tento klub nám před Memoriálem.<br><br>Miroslav Hýža', 7, '0', '2009-12-10 23:00:00'),
(29, 1, 'Ze starých stránek', 'Tréninky s Jardou Kučerou v pátek se ruší pro malý zájem.', 7, '0', '2010-01-09 23:00:00'),
(30, 1, 'Ze starých stránek', 'Upozorňujeme všechny páry na <b>ohlašovací povinnost ukázek</b>. Všechna vystoupení, která děláte (i zdarma) musíte nahlásit vedení klubu.', 7, '0', '2010-01-10 23:00:00'),
(31, 1, 'Ze starých stránek', 'Páteční tréninky s Tomášem Krainou se uskuteční na SGO.', 7, '0', '2010-01-11 23:00:00'),
(32, 1, 'Ze starých stránek', 'Přihlášky do ČSTS, se kterými měl sekretariát ,,problémy", jsou již vyřešeny. Páry: Macháček-Turková, Očenášek-Smyslilová a Vymětal-Mrénková jsou již v soutěžní databázi a mohou se účastnit tanečních soutěží. ID kartičky a soutěžní průkazy Vám budou rozdány hned po dodání. Zatím si berte s sebou na soutěž průkaz totožnosti (nejlépe OP), u páru Očenášek-Smyslilová bude asi potřeba účast rodičů.', 7, '0', '2010-01-19 23:00:00'),
(33, 1, 'Ze starých stránek', 'Páteční tréninky s Tomášem Krainou budou i 29.1., kdy jsou ve školách pololetní prázdniny. Naopak ruší se společné lekce Luie Benýškové, individuální lekce zůstávají.', 7, '0', '2010-01-26 23:00:00'),
(34, 1, 'Ze starých stránek', 'Tréninky s Pavlou Landsfeldovou se uskuteční v PÁTEK odpoledne a v SOBOTU. Pokud některý den nemůžete, napište své požadavky na adresu hyzam @tkolymp.cz. Pokud se neozvete, bude to bráno, že je vám lhostejné, kdy budete mít lekce.', 7, '0', '2010-01-30 23:00:00'),
(35, 1, 'Ze starých stránek', 'Dnešní tréninky s Kamilou se ruší kvůli velkým absencím. Individuální lekce byly přesunuty na středu viz sekce Rozpis tréninků.', 7, '0', '2010-01-31 23:00:00'),
(36, 1, 'Ze starých stránek', 'Nevyzvednuté ID kartičky: Vymětalová, Švarc, Dostálová, Mrázková, Trojan. Domluva na vyzvednutí je idnividuální na tel 737 545 525', 7, '0', '2010-01-31 23:00:00'),
(37, 1, 'Ze starých stránek', 'Byl změněn zítřejší rozpis s Martinem Odstrčilem, zkontrolujte si čas svých lekcí.', 7, '0', '2010-01-31 23:00:00'),
(38, 1, 'Ze starých stránek', 'Páteční tréninky s Terezou Jendrulkovou se přesouvají na Slovanské gymnázium.', 7, '0', '2010-02-02 23:00:00'),
(39, 1, 'Ze starých stránek', 'Páry Homola-Uvízlová, Švarc-Vymětalová a Zárybnický-Hýžová dostanou na víkendovém soustředění s Pavlou Landsfeldovou vrámci přípravy na Mistrovství ČR ve STT jednu hodinu navíc zdarma.', 7, '0', '2010-02-02 23:00:00'),
(40, 1, 'Ze starých stránek', 'Tréninky s Martinem Odstrčilem 9.2. a 15.2. se ruší. Společné hodiny nahradí Miroslav Hýža.', 7, '0', '2010-02-04 23:00:00'),
(41, 1, 'Ze starých stránek', '8.2.2010 se pro nemoc ruší trénink s Kamilou', 7, '0', '2010-02-06 23:00:00'),
(42, 1, 'Ze starých stránek', 'Po dobu jarních prázdnin se ruší všechny pryavidelné tréninky. Vypsány budou tréninky nepravidelné, včetně prektysů a pokud bude zájem, také společných lekcí. Všechny najdete v příštích dnech zde na stránkách.', 7, '0', '2010-02-09 23:00:00'),
(43, 1, 'Ze starých stránek', 'Do tréninkové nabídky byla vložena nabídka tréninků s Kamilou Limberkovou na individuální lekce a na společnou hodinu. Každý, kdo má zájem přijít na společnou hodinu s Kamilou, ať se zde zapíše. Realizace tréninků je podmíněna nejméně 3 dalšími individuálkami.', 7, '0', '2010-02-09 23:00:00'),
(44, 1, 'Ze starých stránek', 'Tento týden bude practise rozdělen na dvě části, ve čtvrtek od 16:00 do 17:00 proběhne STT, od 18:00 do 19:00 LAT.', 7, '0', '2010-02-16 23:00:00'),
(45, 1, 'Ze starých stránek', '<b>Trička dvacet let radosti</b><br><br>Vážení příznivci Tanečního klubu Olymp Olomouc,<br> v rámci výročí 20 let našeho klubu byly zhotoveny trička, jejichž fotky si můžete prohlédnout v odkazech dole. <br><br>  Cena jednoho trička 320 Kč (pořizovací cena bez žádné přirážky), na současné členy Olympu se vztahuje sleva 10 % ze zápisného na druhé pololetí. <br><br>  Velikosti: dámské: S, M, L; pánské: XS, S, M, L, XL <br><br> V případě zájmu si je můžete objednat buďto prostřednictvím Facebooku nebo na hyzam@tkolymp.cz nejpozději do 4. března 2010. <br><br> http://www.facebook.com/photo.php?pid=30601087&l=43d606ae74&id=1106725146<br><br>  http://www.facebook.com/photo.php?pid=30601087&l=43d606ae74&id=1106725146<br><br>  http://www.facebook.com/photo.php?pid=30601087&l=43d606ae74&id=1106725146 ', 7, '0', '2010-02-25 23:00:00'),
(46, 1, 'Ze starých stránek', 'Z důvodu nemoci dnes odpadají tréninky s Kamilou.', 7, '0', '2010-02-28 23:00:00'),
(47, 1, 'Ze starých stránek', '<b>Víkendové soustředění pro juniorské a dospělé páry:<br><br></b>  Miroslav Hýža s Terezou Jendrulkovou nabízejí příští víkend soustředění. Zahrnovalo by následující:<br><br>  <b>2 skupinové lekce STT a LAT (60 min)<br> 2 individuální lekce STT a LAT (45 min)<br> practise STT a LAT (dohromady 90 min)<br>  cena soustředění: 300 Kč/pár<br><br></b>  nebo: <br><b><br> 2 skupinové lekce STT a LAT (60 min)<br> practise STT a LAT (dohromady 90 min)<br> cena: 200 Kč/pár<br><br></b>   Soustředění se uskuteční pouze v případě, že se na společné lekce přihlásí alespoň 6 párů. Akci bychom chtěli uskutečnit v sobotu, pokud by ale většině z vás sobota nevyhovovala, jsme ochotni soustředění přesunout na neděli. Těm, kterým sobota nevyhovuje, se můžou ozvat prostřednictvím Facebooku nebo na hyzam@tkolymp.cz. <br><br>  Zde na webu je v sekci Tréninková nabídka vypsané soustředění, kde můžete potvrdit svoji účast. Udělejte tak<b> nejpozději do úterý (21:00)</b>, poté vytvoříme rozpis nebo akci zrušime - podle počtu zájemců. ', 7, '0', '2010-03-06 23:00:00'),
(48, 1, 'Ze starých stránek', '<b>Tréninky s Tomášem Krainou</b><br><br>  Ve čtvrtek 11.3. na SGO po prektyse (19:00) proběhne krátká schůzka, kde proběhne přerozdělování stávajících lekcí s Tomášem Krainou na středeční a páteční rozpisy.', 7, '0', '2010-03-09 23:00:00'),
(49, 1, 'Ze starých stránek', 'Do rozpisů tréninků bylo přidáno víkendové soustředění. Peníze (300 Kč/pár) přineste na čtvrteční prektys.', 7, '0', '2010-03-09 23:00:00'),
(50, 1, 'Ze starých stránek', 'Tréninky s Kamilou zítra odpadají pro nedostatečný zájem.', 7, '0', '2010-03-20 23:00:00'),
(51, 1, 'Ze starých stránek', 'V pátek v 11:00 se na Slovanském gymnáziu uskuteční společná lekce standardních tanců s Miroslavem Hýžou.<br><br> Budou se probírat věci, které minulý čtvrtek přednášeli v Brně-Modřicích<b> trojnásobní mistři světa Paolo Bosco a Silvia Pitton.</b><br><br> Cena společné lekce bude 100 Kč/pár (nepřihlášení 150 Kč). Hlásit se můžete v sekci Tréninková nabídka.', 7, '0', '2010-03-28 22:00:00'),
(52, 1, 'Ze starých stránek', '<b>Trénujte s nejlepšími!</b><br><br>  Do sekce Tréninková nabídka byly přidány nabídky tréninků se Zdeňkem Landsfeldem a Jardou Kučerou. Nejsou datovány k určitým dnům, termíny budou zajištěny až v případě, že se potvrdí opravdový zájem o společnoé a individuální lekce.<br><br>  Zdeněk Landsfeld - standardní tance: každoroční porotce mistrovství České republiky a významných zahraničních soutěží IDSF. Trenér spousty mistrů České republiky. Nejvzdělanjší český trenér a porotce. Cena lekce (60 min) 600 Kč, páry TK Olymp - exkluzivní cena 400 Kč. <br><br> Jarda Kučera - latinskoamerické tance: každoročně porotuje mistrovství České republiky a spoustu dalších významných soutěží. Je trenérem mnoha mistrů České republiky a významných zahraničních párů. Cena lekce: 800 Kč (45 min). <br><br>  <b>Upozornění trenérské rady TK Olymp Olomouc:<br></b> Není pravdou, že pro začínající páry nemají tyto tréninky smysl. I samotné základy vás nejlépe naučí ti nejlepší trenéři. Proto doporučujeme rozbít prasátko a okusit, jak chutná tanec v podání trenérů, kteří se řadí mezi širší světovou špičku.', 7, '0', '2010-03-28 22:00:00'),
(53, 1, 'Ze starých stránek', '<b>Velikonoční prázdniny</b><br><br>V době velikonočních prázdnin (čtvrtek - pondělí) se nebudou konat pravidelné tréninky <b>KROMĚ PREKTYSU </b>- ve čtvrtek 17:30 (budou se rozdávat trička). Jsou však vypsány nové lekce s Miroslavem Hýžou a Terezou Jendrulkovou, můžete se zapsat.', 7, '0', '2010-03-28 22:00:00'),
(54, 1, 'Ze starých stránek', '<b>Důležité upozornění!</b><br><br>Dnem 21.4. 2010 končí tréninky s Tomášem Krainou. Všem skupinám budou tréninky nahrazeny jinými trenéry ve stejné finanční výši.<br><br> Prosím všechny, kdo mají zájem o <b>PRAVIDELNÉ</b> lekce buďto jedenkrát týdně, nebo jednou za čtrnáct dní,  s Kamilou Limberkovou, ať už v pondělí nebo ve středu, ať se napíší do tréninkové nabídky! Podle zájmu budou domluveny termíny tréninků. ', 7, '0', '2010-04-17 22:00:00'),
(55, 1, 'Ze starých stránek', 'Tento pátek se nekoná společná lekce latiny - skupina dospělí.', 7, '0', '2010-04-22 22:00:00'),
(56, 1, 'Ze starých stránek', 'V pátek 30. dubna povede společnou lekci latiny Tereza Jendrulková v 18:00 na Baletním sále.', 7, '0', '2010-04-25 22:00:00'),
(57, 1, 'Ze starých stránek', 'Následující dva tréninky s Martinem Odstrčilem (27.4. a 4.5.) odpadají. Společné lekce nahradí Miroslav Hýža v obou případech v čase 17:45 až 18:45. ', 7, '0', '2010-04-25 22:00:00'),
(58, 1, 'Ze starých stránek', '<b>Odpadá latina s Kamilou</b> <br><br> Zítra - v pondělí 3.5. se ruší tréninky s Kamilou Limberkovou. Důvodem jsou technické problémy Kamilina auta. Tréninky se přesouvají na středu, kdy by již mělo být všechno v pořádku.  <br><br> Ve středu od 17:15 na malé tělocvičně ZŠ Holečkova proběhne společná lekce pro <b>VŠECHNY JUNIORSKÉ A DOSPĚLÉ PÁRY</b> klubu. Tedy pro ty, co navštěvují pondělní, středční i páteční latinské společné lekce. Vyučovat bude Kamila Limberková.', 7, '0', '2010-05-01 22:00:00'),
(59, 1, 'Ze starých stránek', 'V pátek byly zrušeny individuální lekce: Homola, Hradil a Pánek. Společné lekce povede za Lucku Benýškovou Pavel John.', 7, '0', '2010-05-04 22:00:00'),
(60, 1, 'Ze starých stránek', '<b>Taneční soutěž Olympácký žebříček</b> <br><br> V neděli <b>30.5. 2010</b> v odpoledních hodinách se v KD Sidia uskuteční klubová taneční soutěž. Rozdělená bude na dvě věkové kategorie. První budou Děti I, Děti II a Junioři I. Druhá bude pro Juniory II, Mládež a Hlavní. Počítáme s účastí VŠECH tanečních párů od začátečníků až po ty nejlepší. Soutěžit se bude současně ve standardních a latinskoamerických tancích. Podrobnosti o soutěži najdete v následujících dnech zde na stránkách.  ', 7, '0', '2010-05-05 22:00:00'),
(61, 1, 'Ze starých stránek', '<b>Odpadá pondělní Kamila - přesunuto na středu!</b><br><br>  Z důvodu nemoci je zrušen dnešní trénink s Kamilou, individuální lekce se přesouvají na středu. Od 17:00 na malé tělocvičně ZŠ Holečkova proběhne společná lekce pro <b>VŠECHNY JUNIORSKÉ A DOSPĚLÉ PÁRY</b> klubu. Tedy pro ty, co navštěvují pondělní, středční i páteční latinské společné lekce. Vyučovat bude Kamila Limberková.', 7, '0', '2010-05-16 22:00:00'),
(62, 1, 'Ze starých stránek', '<b>Změna latinských tréninků příští týden</b><br><br>  Příští týden přijede Kamila Limberková ve středu a v pátek. Ve středu 26.5. proběhne společná lekce juniorů od 17:15, v pátek 28.5. proběhne společná lekce <b>VŠECH DOSPĚLÝCH PÁRŮ </b> (pondělní a páteční skupina) od 18 do 19 hodin. Pozorně sledujte rozpisy individuálních lekcí, které budou jinak než obvykle!', 7, '0', '2010-05-18 22:00:00'),
(63, 1, 'Ze starých stránek', 'V pátek 28.5. se nebudou konat lekce s Miroslavem Hýžou. Oběma párům byl trénink přesunut na úterý 25.5. viz rozpis.', 7, '0', '2010-05-18 22:00:00'),
(64, 1, 'Ze starých stránek', 'Podrobné informace o klubové soutěži, která se bude konat 30.5. v KD Sidia se objeví zde v pondělí večer.', 7, '0', '2010-05-18 22:00:00'),
(65, 1, 'Ze starých stránek', 'Byly změněny rozpisy středečních i pátečních individuálních lekcí s Kamilou. ', 7, '0', '2010-05-23 22:00:00'),
(66, 1, 'Ze starých stránek', '<b>Soutěž Olympácký žebříček 2010</b><br><br>  Sál KD Sidia bude otevřený od 12:15, do 13:00 bude probíhat prezence, po 13. hodině budou začínat jednotlivé soutěže. <br><br>Kategorie <b>OLYMP JUNIOR</b> (do 13 let včetně) bude v prvním kole tančit tance Waltz, Quickstep, Chacha, Jive, Polka, ve finále Waltz, Tango, Quickstep, Samba, Chacha, Jive bez přestávky na převlečení. <br><br> Kategorie <b>OLYMP HLAVNÍ</b> (od 14 let) bude tančit tance Waltz, Tango, Valčík, Quickstep, Samba, Chacha, Rumba, Jive) s přestávkami na převlečení v prvním i finálovém kole. <br><br>  Soutěžní oblečení není podmínkou, ale bude zajisté lepší, než obyčejné společenské nebo tréninkové.<br><br>   V obou kategoriích budou nejlepší páry odměněny diplomy, medailemi a poháry. Počítáme s účastí všech našich párů. Pokud nějaký tanec ještě neumíte, nevadí, prostě jej vynecháte.  <br><br> <b>Těšíme se na Vás, všichni jste srdečně zváni. A nezapomeňte: není důležité vyhrát, ale zúčastnit se! ', 7, '0', '2010-05-23 22:00:00'),
(67, 1, 'Ze starých stránek', 'V pátek 4.6. přijede do našeho klubu trénovat Ing. Jaroslav Kučera. Svůj zájem o lekce realizujte v tréninkové nabídce.', 7, '0', '2010-05-23 22:00:00'),
(68, 1, 'Ze starých stránek', 'Až do odvolání se ruší individuální lekce s Miroslavem Hýžou.', 7, '0', '2010-05-24 22:00:00'),
(69, 1, 'Ze starých stránek', 'Od tohoto týdne se ruší pondělní společná hodina juniorů (od 16 hodin). Náhradou je úterní standardní společná lekce s Martinem Odstrčilem na Slovanském gymnáziu od 17:30.', 7, '0', '2010-05-30 22:00:00'),
(70, 1, 'Ze starých stránek', 'V sekci dokumenty naleznete podrobné výsledky Olympáckého žebříčku kategorie OLYMP JUNIOR. Bohužel se ztratil list, na kterém bylo uvedeno jakým párům přísluší jaká startovní čísla, prozatím je tedy výsledková listina čitelná pouze podle čísel. Děkujeme za pochopení.', 7, '0', '2010-05-31 22:00:00'),
(71, 1, 'Ze starých stránek', 'Do sekce dokumenty byly vloženy podrobné výsledky Olympáckého žebříčku kategorie OLYMP HLAVNÍ.', 7, '0', '2010-06-01 22:00:00'),
(72, 1, 'Ze starých stránek', 'Tréninky s Kamilou pro dnešní den se z osobních důvodů ruší. Společnou lekci juniorů nahradí Miroslav Hýža.', 7, '0', '2010-06-01 22:00:00'),
(73, 1, 'Ze starých stránek', 'Tréninky s Kamilou se pro tento týden přesouvají na středu, skupinová hodina bude společná pro PONDĚLNÍ skupinu dospělých a pro středeční skupinu juniorů.', 7, '0', '2010-06-04 22:00:00'),
(74, 1, 'Ze starých stránek', '<b>Pondělní skupina HOBBY DĚTI</b><br><br>  Místo tréninku budete mít ve stejném čase na ZŠ Holečkova ukázky pro rodiče budoucích prvňáčků. Vezměte si soutěžní oblečení.', 7, '0', '2010-06-04 22:00:00'),
(75, 1, 'Ze starých stránek', '<b>Den otevřených dvěří ZŠ Holečkova a Zahradní slavnosti</b><br><br>  Dne 9.6. od 15 do 18 hodin na dvoře ZŠ Holečkova budou probíhat ukázky dětských, juniorských i dospělých párů bez soutěžního oblečení a soutěž StarDance. Všichni, kteří mají zájem přijít podpořit klub, zapište se do tréninkové nabídky, ať víme, s kým můžeme počítat.<br><br>   Z tréninků proběhnou pouze společná a individuální lekce s Kamilou, OSTATNÍ SE RUŠÍ.  ', 7, '0', '2010-06-04 22:00:00'),
(76, 1, 'Ze starých stránek', '<b>Hynčice pod Sušinou 2010</b><br><br>Přihlášky na soustředění Hynčice pod Sušinou 2010 odevzdávejte nejpozději na Slavnostním zakončení taneční sezóny 2009/2010 ve čtvrtek 24.6. 2010. Předběžné přihlášení probíhá na těchto stránkách v sekci Tábory a soustředění, kde si také můžete stáhnout závaznou přihlášku, kterou je třeba odevzdat nejpozději k uvedenému datu.<br><br>  Na přihlášky po termínu nebude brán zřetel. ', 7, '0', '2010-06-04 22:00:00'),
(77, 1, 'Ze starých stránek', '<b>Slavnostní zakončení sezóny 2009/2010</b><br><br>  Ve čtvrtek 24.6. od 17 hodin proběhne v tělocvičně Slovanského gymnázia slavnostní zakončení uplynulé sezóny v podobě posledního klubového prektysu. Sraz je v 16:30. Akce je určená pro rodiče, prarodiče, přátele apod. Zváni jsou všichni členové TK. V tento den již neproběhne tradiční cvičení pilates. Předpokládaný závěr akce je v 19:00.', 7, '0', '2010-06-04 22:00:00'),
(78, 1, 'Ze starých stránek', 'Ruda Stupka a Kristýna Hlavicová získali ve standardních tancích třídu M. Gratulujeme!', 7, '0', '2010-06-11 22:00:00'),
(79, 1, 'Ze starých stránek', 'Dnes z rodinných důvodů odpadají tréninky s Martinem Odstrčilem. O náhradě Vás budeme informovat zde.', 7, '0', '2010-06-14 22:00:00'),
(80, 1, 'Ze starých stránek', 'Páteční tréninky s Luckou Benýškovou se tento týden už NEKONAJÍ. ', 7, '0', '2010-06-19 22:00:00'),
(81, 1, 'Ze starých stránek', 'Fotky ze zakončení sezóny, které nafotil Dalibor Trojan najdete zde: http://www.dalibor-trojan.cz/gallery.php?section=free&subsection=7', 7, '0', '2010-06-24 22:00:00'),
(82, 1, 'Ze starých stránek', 'Přihlášky do Hynčic neodevzdali: Fükö, Daňková, Brunclíková a Kazakovskij. Přihláška je ke stažení v sekci Tábory a soustředění. Je nutné ji odevzdat nejpozději do 10.7.', 7, '0', '2010-07-03 22:00:00'),
(83, 1, 'Ze starých stránek', 'Každou prázdninovou středu od 17 hodin budou v tělocvičně Slovanského gymnázia volné tréninky pro všechny členy klubu.', 7, '0', '2010-07-04 22:00:00'),
(84, 1, 'Ze starých stránek', 'Lekce s Kamilou Limberkovou si zapište nejpozději do středy do večera. Ve čtvrtek budou nabídnuty párům mimo TK Olymp.', 7, '0', '2010-07-12 22:00:00'),
(85, 1, 'Ze starých stránek', 'Do sekce Tréninková nabídka byly vloženy nabídky tréninků s Jardou Kučerou a Kamilou Limberkovou, které se uskuteční na soustředění v Hynčicích. Lekce s ostatními trenéry se budou rozdělovat až na místě. Lekce s Kamilou stojí 400 Kč, lekce s Jardou 800 Kč.', 7, '0', '2010-08-07 22:00:00'),
(86, 1, 'Ze starých stránek', '<b>Platby lekcí </b><br><br> Individuální lekce s Kamčou a Jardou na soustředění v Hynčicích se budou platit hotově u autobusu před odjezdem. Lekce těch, kteří nezaplatí, připadnou k dispozici ostatním zájemcům, na které se zatím nedostalo, pokud budou mít připraveny peníze. ', 7, '0', '2010-08-10 22:00:00'),
(87, 1, 'Ze starých stránek', '<b>HYNČICE 2010 <br><br> Žádáme všechny, kteří ještě nezaplatili soustředění, aby tak neprodleně učinili a při odjezdu předložili doklad o zaplacení (ústřižek ze složenky, výpis z účtu). Ti, co zaplatili do 10.8. doklad předkládat nemusí. ', 7, '0', '2010-08-10 22:00:00'),
(88, 1, 'Ze starých stránek', 'Prosím účastníky soustředění v Hynčicích nad 15 let o napsání ankety k tématu NUDA.Také mi napište věk a jsteli studenti.Předem děkuji Benýšková. (BenyskovaRenata@seznam.cz)', 7, '0', '2010-08-29 22:00:00'),
(89, 1, 'Ze starých stránek', 'První záříjový trénink ve formě prektysu se uskuteční ve čtvrtek v 17 hodin na Slovanském gymnáziu. Srdečně zvány jsou všechny páry od dětí až po dospělé. ', 7, '0', '2010-08-30 22:00:00'),
(90, 1, 'Ze starých stránek', '<b>8.9.2010 - ZŠ Holečkova<br><br></b>         - 15.45 schůze rady<br>         - 16.30 burza - malá tělocvična        <br>         - 17.00 členská schůze - třída vedle malé tělocvičny<br>---------------------------<br><br>    30.9.- NÁBOR- SGO v 17hod.- NULTÁ LEKCE<br><br>  Od 13.9. do 26.9. budou letáky na nábor v tramvajích a autobusech.', 7, '0', '2010-08-31 22:00:00'),
(91, 1, 'Ze starých stránek', '<b>Tréninky na týden od 6.9. 2010: <br><br>  Pondělí - baletní sál</b><br> 16:00 společná DĚTI<br> 17:00 společná LAT junioři a dospělí<br><br>  <b>Úterý - SGO<br></b> 17:30 společná STT junioři a dospělí<br><br>  <b>Středa - baletní sál:<br> </b> 17:15 společná LAT JUNIOŘI<br> 18:00 společná LAT DOSPĚLÍ<br><br>  <b>Čtvrtek - SGO:</b><br> 17:00 practise děti, junioři a dospělí<br><br> Ostatní se dozvíte na členské schůzi.', 7, '0', '2010-09-04 22:00:00'),
(92, 1, 'Ze starých stránek', 'Tréninky s Kamilou se oproti původní domluvě přesouvají pro tentokrát na středu. Společná hodina LAT v pondělí bude.', 7, '0', '2010-09-04 22:00:00'),
(93, 1, 'Ze starých stránek', '<b>INFORMACE Z ČLENSKÉ SCHŮZE</b> <bR><br><b>Tréninkové skupiny:</b><br><br> ŽLUTÁ<br>  - junioři + dospělí registrovaní:<br> pondělí/středa na ZŠ Holečkova od 17:00 – Kamila<br> úterý na SGO od 17:30: Odstrčil<br> čtvrtek na SGO 16:15 – 19:00:  pilates, practise<br> pátek: individuálky Mirek, Terka, Lucka, Pavel, příležitostně Kučera<br> cena na pololetí : 2500 Kč (pouze STT/LAT: 1700 Kč)<br> <br> ČERVENÁ<br>  - děti +junioři hobby: <br> pondělí na ZŠ Holečkova od 16:00: Mirek<br> čtvrtek na SGO  16:15 – 19:00: pilates, practise<br> pátek na ZŠ Holečkova 16:00: Lucka/Terka<br>  - individuálky Mirek, Terka, Lucka, Pavel<br> cena na pololetí : 1500 Kč<br> <br> MODRÁ<br>  -druháci + třeťáci<br> úterý, čtvrtek: 12:30 – 14:00<br> cena na pololetí : 800 Kč<br> <br> ZELENÁ<br>  - páry z TŠ Progress<br> středa na SGO 18:00 – 19:30: Marek a Zuzka<br> čtvrtek na SGO 16:15: pilates, practise<br> - možnost navštěvovat společné tréninky žluté skupiny za poplatek 50 Kč/osobu<br> cena na pololetí: 1200 Kč<br> <br> SAMOSTATNÝ KLUBOVÝ POPLATEK <br> vstup na sály, možnost tréninků za klubové ceny a poplatek 50 Kč/osobu za společnou hodinu s Odstrčilem a Limberkovou, v případě, že pár vystupuje za TK OLYMP!!<br> cena na pololetí: 500 Kč<br> <br><br> <b>Nominace na MČR družstev: </b><br> 9.-10- října v Praze<br><br>  junior  STT:<br> Homola – Uvízlová<br> Očenášek – Smyslilová<br> Švarc – Vymětalová<br> Zárybnický –  Dovčiaková<br> kapitán: Miroslav Hýža<br><br> junior LAT:<br> Anderle – Chromcová<br> Homola – Uvízlová<br> Švarc – Vymětalová<br> Zárybnický – Dovčiaková<br> kapitán: Tereza Jendrulková + Lucie Benýšková<br><br> dospělí LAT:<br> Hýža – Hýžová<br> Kůla – Jendrulková<br> Pastrňák – Brunclíková<br> Vymětal – Mrténková<br> kapitán: Kamila Limberková<br> - náhradníci: Ru. Stupka – Hlavicová, Fükö – Daňková, Macháček – Turková….pokud vytančí třídu B<br> - příspěvek od klubu na tréninky 2000 Kč na pár a skupinu tanců <br> - hledáme v Praze ubytování<br>  - 17.9., 8.10., tréninky s Kučerou<br>  <br> <br> <b>Ostatní informace:</b><br> Zájem o standardní lekce s Pavlou nebo Zdeňkem Landsfeldovými<br> Nábor – 30.9. místo prektysu nultá lekce začátečníků<br> 17. listopadu(středa) Memoriál – je potřeba sehnat sponzory, zapojení do organizace – Renáta Benýšková<br> Klubové soupravy – nákup/ušití, potisk… hledáme levné a kvalitní – návrhy?<br> Letní soustředění – hledáme nový areál za vhodnou cenu <br> Ukázky – ohlašovací povinnost (i zadarmo pro babičku)<br> 	- kdo si sežene ukázky sám, dostane 3 z celkové ceny<br> 	- kdo je vyslán na ukázky klubem, dostane 1 z celkové ceny<br> Povinnost účastnit se klubových akcí – Olympácký žebříček, nábor, Memoriál, dětský den, ukázky, atd.<br> Slevenky na taneční obuv v obchůdku Tali Dance<br> Odměny ve formě lekcí za docházku na practise a pilates<br> Individuální lekce s externími trenéry je možné brát pouze v případě účasti na společné lekci<br> Povinnost sledovat klubový web www.tkolymp.cz – sekce upozornění<br> 	- všichni musí mít své přihlašovací jméno a heslo –na starosti Mirek: 737 545 525, hyzam@tkolymp.cz<br> ', 7, '0', '2010-09-10 22:00:00'),
(94, 1, 'Ze starých stránek', 'Individuální lekce označené písmeny DR jsou hrazeny klubem vrámci příspěvku na přípravu MČR družstev.', 7, '0', '2010-09-10 22:00:00'),
(95, 1, 'Ze starých stránek', '<b>Doplnění tanečních skupin</b><br><br> ORANŽOVÁ<br> - taneční přípravka pro předškoláky<br> úterý - baletní sál: 15:30 - 16:30 Marie Hýžová<br> pátek - malá tělocvična: 15:00 - 16:00 Lucka Benýšková<br> cena na pololetí: 1500 Kč', 7, '0', '2010-09-11 22:00:00'),
(96, 1, 'Ze starých stránek', 'Ve středu v 16:30 bude na ZŠ Holečkova krátká schůze účastníků MČR družstev (u mladších 18 let jejich rodičů). Prosíme, přijďte všichni.', 7, '0', '2010-09-11 22:00:00'),
(97, 1, 'Ze starých stránek', 'Členské příspěvky na II. pololetí je nutné zaplatit do 30. 9. 2010!!!', 7, '0', '2010-09-12 22:00:00'),
(98, 1, 'Ze starých stránek', 'Martin Odstrčil se moc omlouvá, ale z osobních důvodů musí na poslední chvíli zrušit dnešní tréninky. Společnou hodinu ve stejném čase převezme Miroslav Hýža', 7, '0', '2010-09-13 22:00:00'),
(99, 1, 'Ze starých stránek', '<b>Cesta a ubytování doprovodu na družstva</b><br><br>Všem členům, kteří chtějí jet podpořit naše družstva do Prahy na mistrovství České republiky, nabízíme možnost společné cesty vlakem a společného ubytování na hostelu. Cena noclehu na čtyřlůžkovém pokoji je 250 Kč a na třílůžkovém pokoji 290 Kč a ve dvojlůžkovém 330 Kč. Cena jedné cesty by neměla přesáhnout 200 Kč. V případě vašeho zájmu kontaktuje Marii Hýžovou nebo Miroslava Hýžu. NEJPOZDĚJI DO PÁTKU 17.9. 2010', 7, '0', '2010-09-14 22:00:00'),
(100, 1, 'Ze starých stránek', '<b>Upozornění pro všechny páry červené skupiny:</b><br><br>  Vaše pondělní tréninky budou oproti původnímu plánu posunuty na 17 hodin. ', 7, '0', '2010-09-18 22:00:00'),
(101, 1, 'Ze starých stránek', 'Taneční soutěž Memoriál Dr. Miroslava Hýži byla přesunuta na sobotu 20.11. 2010 ', 7, '0', '2010-09-20 22:00:00'),
(102, 1, 'Ze starých stránek', '<b>Workshop s Pavlou Landsfeldovou a Ing. Zdeňkem Landsfeldem</b><br><br>  Dne 19.11. 2010 proběhne workshop se špičkovými trenéry standardních tanců Pavlou Landsfeldovou a ing. Zdeňkem Landsfeldem, kteří budou následující den v porotě na Memoriálu Dr. Miroslava Hýži.<br><br>   Workshop zahrnuje jednu individuální lekci se Zdeňkem a jednu s Pavlou (45 min). Cena je 600 Kč na pár. Hlásit se můžete v tréninkové nabídce do 1.11.', 7, '0', '2010-09-20 22:00:00'),
(103, 1, 'Ze starých stránek', '<b>Dospělácké družstvo startovat nebude</b><br><br>  Z důvodu nedostatečné připravenosti některých párů byla zrušena účast našeho latinského družstva, které mělo startovat ma Mistrovství ČR družstev 2010. Zároveň se ruší vyplácení dvoutisícových odměn na tréninky pro všechny páry tohoto družstva (složení viz níže).', 7, '0', '2010-09-20 22:00:00'),
(104, 1, 'Ze starých stránek', '<b>DŮLEŽITÉ UPOZORNĚNÍ</b><br><br>Žádáme všechny členy našeho klubu, aby si každou neděli večer zkontrolovali tréninky na příští týden. Vyhnete se tak nepříjemným omylům. Zároveň upozorňujeme členy žluté skupiny, že společná hodina s Kamilou Limberkovou bude bývat ve středu NEBO v pondělí, tak, jak jsme se domluvili na schůzi. ', 7, '0', '2010-09-20 22:00:00'),
(105, 1, 'Ze starých stránek', 'Tréninky s Martinem Odstrčilem se z důvodu služební cesty do Lucemburska pro příští týden přesouvají na pátek', 7, '0', '2010-09-21 22:00:00'),
(106, 1, 'Ze starých stránek', 'Nedělní tréninky s Miroslavem Hýžou se uskuteční na ZŠ Holečkova ve velké tělocvičě.', 7, '0', '2010-09-22 22:00:00'),
(107, 1, 'Ze starých stránek', 'V úterý v 16:30 se na Slovanském gymnáziu uskuteční practise, který se z důvodu náboru nebude konat ve čtvrtek. Tento týden se také neuskuteční cvičení pilates. ', 7, '0', '2010-09-24 22:00:00'),
(108, 1, 'Ze starých stránek', 'Číslo účtu pro platbu příspěvků je: 1806875329-0800. Upozorňujeme, že příspěvky je nutné zaplatit nejpozději do 30.9. 2010', 7, '0', '2010-09-25 22:00:00'),
(109, 1, 'Ze starých stránek', '<b>Soustředění Olomoucké divize ČSTS</b><br><br>  Olomoucká divize ČSTS pořádá pro své členy ve dnech 23. a 24. října soustředění STT s Marcelem a Lenkou Gebertovými a LAT s Jardou Kučerou. V sobotu se od 9:00 do 12:15 uskuteční seminář LAT, v neděli ve stejném čase seminář STT. Po oba dva dny budou probíhat individuální lekce, zájemci se mohou zapsat v sekci Tréninková nabídka do 17.10. Lekce s Marcelem a Lenkou stojí 600 Kč/45 min, lekce s Jardou 800 Kč/45 min.  <br><br> <b>Oba čtyřhodinové dopolední semináře jsou pro REGISTROVANÉ ČLENY(!) zdarma.</b>', 7, '0', '2010-09-26 22:00:00'),
(110, 1, 'Ze starých stránek', 'Zítřejší tréninky s Martinem Odstrčilem se ruší. Důvodem je stávka leteckých společností, kvůli nimž se Martin nestihne vrátit do České republiky včas. Společnou hodinu nahradí ve stejném čase Miroslav Hýža', 7, '0', '2010-09-29 22:00:00'),
(111, 1, 'Ze starých stránek', 'Společná hodina s Kamilou bude tento týden v pondělí od 17 hodin.', 7, '0', '2010-10-02 22:00:00'),
(112, 1, 'Ze starých stránek', 'Dnešní tréninky s Pavlem Johnem se pro nemoc ruší!', 7, '0', '2010-10-07 22:00:00'),
(113, 1, 'Ze starých stránek', 'Společné hodiny s Kamilou budou od příštího týdne rozdělené. V pondělí od 17 hodin budou trénovat páry hobby a třídy D a C, ve středu potom páry tříd B, A a M.', 7, '0', '2010-10-07 22:00:00'),
(114, 1, 'Ze starých stránek', '<b>PÁTÉ MÍSTO VE STANDARDU A BRONZ V LATINĚ!!!<br><br></b>  O víkendu se naše juniorská družstva zúčastnila Mistrovství České republiky družstev 2010, které se konalo v Praze. Nae standardní družstvo startovalo ve složení:<br><br>  Marek Švarc - Monika Vymětalová<br> Matěj Očenášek - Marie Smyslilová<br> Jakub Zárybnický - Kristýna Dovčiaková<br> Dominik Homola - Mirka Uvízlová<br> kapitán: Miroslav Hýža<br><br>  V celé České republice se nakonec našlo pouhých 6 klubů, které byly schopny postavit družstvo, naši junioři nakonec obsadili krásné páté místo, přestože od stříbrných medailí nás dělilo pouhých 7 bodů. <br><br>  Latinské družstvo startovalo o den později ve složení:<br>  Jiří Anderle - Veronika Chromcová<br> Dominik Homola - Mirka Uvízlová<br> Jakub Zárybnický - Kristýna Dovčiaková<br> Marek Švarc - Monika Vymětalová<br> kapitáni: Tereza Jendrulková a Miroslav Hýža<br><br>  V napínavém finále jsme nakonec obsadili vynikající medailovou, konkrétně bronzovou pozici. <br><br>Děkujeme celé výpravě, která čítala přes třicet členů za jejich obětavost a podporu. ', 7, '0', '2010-10-10 22:00:00'),
(115, 1, 'Ze starých stránek', '<b>Ceník individuálních lekcí trenérů TK Olymp</b><br><br>  Mgr. Marie Hýžová 100 Kč/60 min - STT,LAT<br> Lucie Benýšková 100 Kč/60 min - STT, LAT<br> Bc. Pavel John 100 Kč/60 min - STT, LAT<br> Tereza Jendrulková 100 Kč/45 min - LAT<br> Miroslav Hýža 160 Kč/60 min - STT<br> Tomáš Kraina 200 Kč/45 min - LAT<br> Pavla Landsfeldová 350 Kč/60 min - STT<br> Ing. Zdeněk Landsfeld 400 Kč/60 Min - STT<br> Kamila Limberková 400 Kč/45 min - LAT<br> Martin Odstrčil 500 Kč/45 min - STT<br> Ing. Jaroslav Kučera 800 Kč/ 45 min - LAT <br><br> V případě Vašeho zájmu o individuální lekci s kterýmkoliv trenérem z tohoto seznamu, pokud není v sekcích Tréninková nabídka nebo Rozpis tréninků, kontaktujte Miroslava Hýžu na hyzam@tkolymp.cz nebo 737 545 525.', 7, '0', '2010-10-10 22:00:00'),
(116, 1, 'Ze starých stránek', '<b>Doplňující informace k workshopu Olomoucké divize 23.-24.10. 2010<br><br></b>  Obou seminářů - jak STT, tak LAT se mohou účastnit i hobby páry klubů, které přísluší k Olomoucké divizi po předložení potvrzení od klubu, že jsou jeho členy a zaplacení poplatku 75 Kč/osobu, za každý seminář. V případě vašeho zájmu o vydání tohoto potvrzení kontaktujte Miroslava Hýžu na hyzam@tkolymp.cz. Členové TK Olymp, kteří mají partnera/ku z jiné divize se soustředění mohou účastnit také!', 7, '0', '2010-10-10 22:00:00'),
(117, 1, 'Ze starých stránek', 'Dnešní společnou hodinu s Kamilou naradí Miroslav Hýža od 17 hodin na baleťáku.', 7, '0', '2010-10-10 22:00:00'),
(118, 1, 'Ze starých stránek', 'Páteční tréninky s Pavlem Johnem byly pro nemoc zrušeny.', 7, '0', '2010-10-12 22:00:00'),
(119, 1, 'Ze starých stránek', '<b>WORKSHOP Olomoucké divize ČSTS</b><br><br>V sobotu se od 9:00 do 12:15 uskuteční seminář LAT s Jardou Kučerou, v neděli ve stejném čase seminář STT s Marcelem a Lenkou Gebertovými. Rozpis individuálních lekcí dle Vašich rezervací v tréninkové nabídce bude zveřejněn zítra (pondělí). Pro registrované páry je workshop <b>PO PŘEDLOŽENÍ ID KARTIČKY</b> zdarma. Hobíci platí 75 Kč/osobu a den. <br><br> Workshop se uskuteční v KD Lola (Sudova 16, Olomouc). Buďte zde pokaždé v 8:40. <br><br>Byly přihlášeny páry (za jménem partnera automaticky chápejte i partnerku):<br><br>  registrovaní: Hýža, Pastrňák, Ra. Stupka, Ru. Stupka, Vymětal, Vymazal, Švarc, Očenášek, Homola, Pecha, Drozd, Navrátil, Jendrulek, Zárybnický, Anderle, Macháček, Fükö<br><br>   hobby: Hradil, Palík, Hanzlík, Řezníček, Bečka, Fiala, Vašek<br><br>  Pokud by někdo na seznamu chyběl nebo byl navíc, kontaktujte M. Hýžu. ', 7, '0', '2010-10-16 22:00:00'),
(120, 1, 'Ze starých stránek', 'Upozorňujeme všechny páry, které si zarezervovali lekce na workshop OD, aby si zkontrolovaly časy svých lekcí v rozpisu, který je ke stažení v sekci Dokumenty.<br><br>  Pokud by některý pár nemohl na svou lekci např. ze zdravotních důvodů přijít nebo by chtěl lekci navíc (volná místa viz rozpis), kontaktujte organizátory workshopu, kterými jsou Igor Opravil (opraviligor@seznam.cz) a Jitka Šoustalová (jitka@kstquick.cz). ', 7, '0', '2010-10-18 22:00:00'),
(121, 1, 'Ze starých stránek', '<b>Objednávka učebnic</b><br> <br> Všichni zájemci o knihy Ing. Zdeňka Landsfelda Technika standardních tanců a Technika latinskoamerických tanců se do zítřka mohou psát do nabídky zde na webu. Předběžná cena je 300, resp. 420 Kč. Hromadnou objednávku obstará Ruda Stupka, všechny dotazy tedy směřujte na jeho osobu.', 7, '0', '2010-10-20 22:00:00'),
(122, 1, 'Ze starých stránek', 'Organizátoři workshopu OD nám zaslali nový rozpis lekcí. Stáhněte si jej v sekci dokumenty a zkontrolujte svoje individuální lekce.', 7, '0', '2010-10-21 22:00:00'),
(123, 1, 'Ze starých stránek', '<b>DŮLEŽITÉ!</b><br> <br>  Vážení tanečníci, během měsíce listopadu se budou na společných lekcích ST + LA vybírat příspěvky ČSTS na r. 2011. <b>!</b><br> <br>   <b>Děti do 12 let:    1 100 Kč!</b><br> <br>  <b>ostatní:           1 350 Kč!</b><br> <br>  <b>noví členové:      -"-     +  50 Kč!</b><br> <br>  !!!Prosím o vyřízení této povinnosti v uvedeném termínu!!! Děkuji Jendrulková', 7, '0', '2010-10-22 22:00:00'),
(124, 1, 'Ze starých stránek', 'Objednávka učebnic: <br><br>  Dnešním dnem jsem podal objednávku na taneční publikace (Technika STT a Technika LAT). Není už tedy možné objednávat. Výsledná částka se nepatrně změnila: STT 310 Kč a LAT 430 Kč. Omlouvám se za změnu, ale důvodem jsou přepravní náklady. Prosím veškeré zájemce v sekci tréninková nabídka, aby mi tuto částku v nejbližší době uhradili.  Peníze budu vybírat já během tohoto týdne na těchto akcích:<br> Út - společná STT<br> St - společná LAT<br> Čt - Practise<br><br>  Děkuji Stupka ', 7, '0', '2010-10-23 22:00:00'),
(125, 1, 'Ze starých stránek', 'Treninky Martina Odstrčila se dnes ze zdravotních důvodů ruší.', 7, '0', '2010-10-25 22:00:00'),
(126, 1, 'Ze starých stránek', 'Počínaje dnešním dnem platí zákaz vstupu do tělocvičny SGO bez vědomí M. Hýži. Minulý víkend po nás zůstal nepořádek a otevřená okna v šatně, což byla pro vedení SGO poslední kapka. Zákaz platí až do odvolání. <br><br>  Vedené tréninky v úterý, středu, čtvrtek a pátek platí i nadále.', 7, '0', '2010-10-25 22:00:00'),
(127, 1, 'Ze starých stránek', 'Ve čtvrtek 28.10. (státní svátek) se nekoná pilates, practise ani latina pro ženy a dívky.', 7, '0', '2010-10-25 22:00:00'),
(128, 1, 'Ze starých stránek', 'Po dohodě s Martinem Odstrčilem byl vypsán náhradní rozpis lekcí na páteční dopoledne včetně společné hodiny. Můžete si tedy zarezervovat lekce.', 7, '0', '2010-10-25 22:00:00'),
(129, 1, 'Ze starých stránek', 'Páteční vedené lekce s Luckou Benýškovou a Terezou Jendrulkovou se z důvodu podzimních prázdnin nekonají.', 7, '0', '2010-10-25 22:00:00'),
(130, 1, 'Ze starých stránek', 'Páteční trénink s Martinem Odstrčilem se pro nedostatečný zájem ruší.', 7, '0', '2010-10-26 22:00:00'),
(131, 1, 'Ze starých stránek', 'Víkendové trénování na SGO(30.-31.10):<br><br>  sobota: 16:00 - 18:00<br> neděle: 14:00 - 16:00<br><br>  Mimo tyto hodiny je vstup do tělocvičny zakázán.', 7, '0', '2010-10-27 22:00:00'),
(132, 1, 'Ze starých stránek', 'Středeční tréninky s Kamilou Limberkovou se pro tento týden přesouvají na pátek.', 7, '0', '2010-10-31 23:00:00'),
(133, 1, 'Ze starých stránek', '<b>NOVÉ ROZPISY</b> <br><br> Vážení tanečníci, od příštího týdne drobně pozměňujeme organizaci rozpisů tréninků. Ty bude od nynějška rozepisovat Veronika Mrténková. Veškeré připomínky k lekcím tedy směřujte na její osobu (mobil: 725555642). <br><br> Zároveň je potřeba, abyste jí na mail (verunka.tanecnice@seznam.cz) poslali následující informace:<br>  - počet lekcí, o které máte zájem pravidelně s trenéry: Odstrčil, Limberková, Hýža, Benýšková<br>  - periodu opakování (1x týdně, 2x týdně, jednou za 14 dní)<br>  - časové rozmezí, ve kterém se můžete na lekci dostavit (napište více možností, pokud napíšete pouze jednu, může se stát, že Váš požadavek nebudeme schopni splnit).<br><br>  Zapisování pravidelných individuálních lekcí probíhá letos už podruhé (poprvé na členské schůzi v září). Je tomu tak proto, že lekce byly často napsané nezodpovědně a docházelo k jejich častému rušení. <br><br> <b>Pokud tyto požadavky včas nezašlete, může se stát, že na Vás individuální lekce od příštího týdne nevyjdou.</b>', 7, '0', '2010-11-04 23:00:00'),
(134, 1, 'Ze starých stránek', '<b>OBCHOD DANCE POINT</b><br> Během minulého týdne jsem sjednal obchodní spolupráci s tanečním obchodem Dance-point. Jsem tedy schopen získat veškeré zboží v tomto obchode (chrániče, boty, kartáče, DVD, literatura,...).<br> Na internetové adrese <b>www.dance-point.cz</b> získáte podrobnější informace o obchodě.<br> Cena zboží bude stejná, jak je uvedena v internetovém obchodě. Nemusíte tedy nikam dojíždět a platit poštovné.<br> Dodání se bude lišit dle počtu objednaného zboží. (Boty a větší objednávky budou expedovány rychleji)<br> Pokud-li máte zájem o některé zboží, kontaktujte mě na <b>stupka.rudolf@email.cz</b>, nebo zprávy Facebook.<br><br> Dostal jsem první várku Technika STT a Technika LAT (bohužel nebylo na skladě dostatek kusů a muselo se objednat).<br> Zbytek bych měl dostat během přístího týdne.<br> Upozorňuji všechny, kteří techniku dostali, aby si ji přinesli na přístí společnou STT. Budeme s nimi pracovat<br><br> Děkuji<br> Stupka', 7, '0', '2010-11-05 23:00:00'),
(135, 1, 'Ze starých stránek', 'V pátek 12.11. se uskuteční tréninky s Jardou Kučerou. Společná hodina pro žlutou skupinu bude od 17 hodin. Individuální lekce si můžete zapisovat do středy v sekci tréninková nabídka.', 7, '0', '2010-11-05 23:00:00'),
(136, 1, 'Ze starých stránek', 'Počínaje dnem 15.11. se budou na Slovanském gymnáziu konat volné tréninky každé pondělí od 17 do 19 hodin.', 7, '0', '2010-11-13 23:00:00'),
(137, 1, 'Ze starých stránek', 'Tréninky s Kamilou Limberkovou dnes odpadají z osobních důvodů. Přijďte všichni na volný trénink s Slovanské gymnáziu od 17 do 19 hodin. Povedou jej Martin Pastrňák a Rudolf Stupka.', 7, '0', '2010-11-14 23:00:00'),
(138, 1, 'Ze starých stránek', 'Nabídka tréninků s Kamilou Limberkovou na pátek 19.11. bude k dispozici do úterý. Trénink se uskuteční pouze v případě, že bude zapsáno nejméně 5 lekcí.', 7, '0', '2010-11-14 23:00:00'),
(139, 1, 'Ze starých stránek', 'Pravidelné tréninky s Mirkem<br><br>  Podle Vašich požadavků jsem vytvořil rozpis individuálek na pátek 26.11. Takto by měl rozpis vypadat pravidelně. Zatím se mi nepodařilo najít takové množství času, abych uspokojil všechny zájemce. Nepravidelně se ale můžete zapisovat na víkendové tréninky buďto se mnou nebo Pavlem Johnem.<br><br> V případě, že Vám vypsaný čas nebude vyhovovat, kontaktujte mě na hyzam@tkolymp.cz, 737 545 525 nebo osobně. Pokud se neozvete, budu počítat s tím, že termín vyhovuje a na tréninky se dostavíte.<br><br>Miroslav Hýža', 7, '0', '2010-11-14 23:00:00'),
(140, 1, 'Ze starých stránek', 'Workshop s Pavlou Landsfeldovou a Ing. Zdeňkem Landsfeldem se bude platit v úterý na společné hodině s Martinem Odstrčilem. Cena je 600 Kč/pár.', 7, '0', '2010-11-14 23:00:00'),
(141, 1, 'Ze starých stránek', '<p><b>Učebnice</b></p> Omouvám se, ale nedokázal jsem během minulého týdne získat učebnice STT a LAT.<br> Chyba nastala při přeposílání zásilky na jiný sklad. Během tohoto týdne by už měly přijít.<br> Omlouvám se všem, kteří knížku ještě nedostali, ale snažím se vše urychlit.<br>  Děkuji<br> Stupka ', 7, '0', '2010-11-14 23:00:00'),
(142, 1, 'Ze starých stránek', 'Tréninky s Martinem Odstrčilem odpadají z osobních důvodů. Společnou hodinu nahradí od 17:30 Miroslav Hýža. Tělocvična bude přístupná od 16:00.', 7, '0', '2010-11-14 23:00:00'),
(143, 1, 'Ze starých stránek', 'Zkontrolujte si změněný harmonogram soutěže v Přerově. Informace pro hobby páry naleznete v sekci Dokumenty', 7, '0', '2010-11-15 23:00:00'),
(144, 1, 'Ze starých stránek', 'V pátek 19.11.2010 v 18.30 hod.v RCO příprava sálu na sobotní soutěž - prosím o pomoc při jeho přípravě - pak bude potřeba po sobotní soutěži i úklid sálu. Předem děkuji za pomoc. Dále prosím rodiče, kteří by chtěli pomoci s přípravou soutěže - potřebuji obsadit pokladnu, hlavně v rozmezí 11-14.30 hod. A s přípravou občerstvení ozvěte se prosím na tel.č.- 776 728 152 .Každý kdo nějakým způsobem pomůže ohledně přípravy soutěže bude mít u pokladny na své jméno nachystanou volnou vstupenku - vstupenky  /počet -  podle toho s čím vším pomůže/.      Děkuji Benýšková Renáta', 7, '0', '2010-11-16 23:00:00'),
(145, 1, 'Ze starých stránek', 'Právě jsem aktualizoval časový harmonogram soutěže. Zkontrolujte si jej v propozicích na: http://csts.cz/?q=cs/kalendar-soutezi/propozice/4307, nebo podrobněji v dokumentech na našem webu. Miroslav Hýža', 7, '0', '2010-11-18 23:00:00'),
(146, 1, 'Ze starých stránek', 'Dne 2.12. 2010 se ve Velké tělocvičně ZŠ Holečkova uskuteční Mikulášská besídka TK Olymp Olomouc. V tento den se nebude konat practise ani cvičení pilates. Všechny páry TK Olymp budou dělat ukázky.', 7, '0', '2010-11-23 23:00:00'),
(147, 1, 'Ze starých stránek', 'Tento pátek odpadají tréninky s M. Hýžou z důvodu nemoci. Příští pátek nebudou rovněž, protože tu bude trénovat Jarda Kučera.', 7, '0', '2010-11-24 23:00:00'),
(148, 1, 'Ze starých stránek', 'Volné tréninky na SGO se uskuteční v sobotu od 14-16h. Na neděli se ještě domlouváme, čas upřesnim během zítřka, ale bude se jednat o dopoledne (11-13). <br> Případné dotazy směřujte na mě<br> Stupka ', 7, '0', '2010-11-25 23:00:00'),
(149, 1, 'Ze starých stránek', 'Nedělní trénink se ruší. Všem se omlouvám, ale nestíhám. Případné informace mi pište na mobil.<br> Stupka', 7, '0', '2010-11-26 23:00:00'),
(150, 1, 'Ze starých stránek', 'Byl změněn rozpis tréninků s Martinem Odstrčilem, prosím zkontrolujte si svoje hodiny, pokud se odhlásí více jak jeden pár, tréninky se zruší!!!', 7, '0', '2010-11-27 23:00:00'),
(151, 1, 'Ze starých stránek', 'V pondělí se volné tréninky na SGO uskueční od 17-19h.<br> Stupka', 7, '0', '2010-11-27 23:00:00'),
(152, 1, 'Ze starých stránek', 'Z důvodu sněhové kalamity na Bruntálsku odpadají dnešní tréninky s Kamilou Limberkovou. Společná hodina od 17 hodin na baletním sále se konat BUDE!', 7, '0', '2010-11-28 23:00:00'),
(153, 1, 'Ze starých stránek', '<b>Překvapení pro Martina</b><br> Minulou neděli Martin Odstrčil slavil narozeniny. Bylo by určitě fajn, kdybychom dali hlavy dohromady a něčím ho na společné překvapili.<br> Přijímám každý návrh. Asi je dost pozdě dávat dohromady nějaký společný dárek, ale bylo by dobré, kdyby každý něco donesl.<br> S případnými návrhy mě kontaktujte a určitě na Martina nezapomeňte.<br> Děkuji Stupka', 7, '0', '2010-11-28 23:00:00'),
(154, 1, 'Ze starých stránek', 'V pátek v 17 hodin se v baletním sále uskuteční společná hodina s Jardou Kučerou pro žlutou skupinu.', 7, '0', '2010-11-29 23:00:00'),
(155, 1, 'Ze starých stránek', '<b>Mikulášská<br><br></b> již dříve jsme psali, že ve čtvrtek 2.12. se uskuteční Mikulášská besídka TK Olymp. Bude probíhat od 16:30 na Velké tělocvičně ZŠ Holečkova. Sraz tanečníků je v 16:15. Prosíme VŠECHNY naše páry, aby si s sebou vzaly taneční oblečení, partnerky chrániče podpatků a byly připraveny na vystoupení. ', 7, '0', '2010-11-30 23:00:00'),
(156, 1, 'Ze starých stránek', '<b>Červená skupina - ukázky</b><br><br> Žádáme všechny páry červené skupiny, aby si s sebou na páteční trénink vzali oblečení na ukázky. Trénink skončí o 15 minut dříve, následně se budeme přesouvat na házenkářskou halu Zory (na 17:15), kde děti vystoupí. Zároveň bychom chtěli poprosit rodiče, aby pomohli převézt děti ze ZŠ Holečkova na Zoru. Děkujeme.', 7, '0', '2010-11-30 23:00:00'),
(157, 1, 'Ze starých stránek', 'Z důvodu nemoci se ruší nedělní tréninky s Miroslavem Hýžou.', 7, '0', '2010-12-02 23:00:00'),
(158, 1, 'Ze starých stránek', 'Tréninky v sobotu se uskuteční:<br> sobota: 14-16h<br> neděle: 11-13h<br> Nedělní trénky se mohou zrušit, záleží na teplotě v tělocvičně.<br> Kontrolujte zítra večer web !!<br> Stupka ', 7, '0', '2010-12-02 23:00:00'),
(159, 1, 'Ze starých stránek', 'Tréninky se zítra na SGO ruší. Omlouvám se všem.<br> Případné informace viz. mobil.<br> Stupka', 7, '0', '2010-12-03 23:00:00'),
(160, 1, 'Ze starých stránek', '<b>VÁNOČNÍ SOUSTŘEDĚNÍ</b> <br><br> Ve spolupráci s DSP Kometa Brno jsme pro Vás připravili Vánoční soustředění Prštice 2010. Uskuteční se ve dnech 27.-29. prosince v Pršticích u Brna.  <br><br> Soustředění je PRO VŠECHNY páry našeho klubu. Od nejmenších po nejstarší, od HOBBY párů po třídu M. Páry budou rozděleny do výkonnostních skupin a budou s nimi pracovat jak naši (Tereza Jendrulková, Miroslav Hýža, Kamila Limberková) tak externí lektoři (Martin Odstrčil, Landsfeldovi, Jan Tománek, Petr Odstrčil a další). Na děti bude po celou dobu soustředění dohlížet Verča Frýdecká, kterou znají z letního tábora.  <br><br> Denní náplň: rozcvička, 2x společná lekce (1x STT, 1x LAT), 2x practise (90 minut), volné trénování, individuální lekce  Cena: kurzovné 900, ubytování a strava 900 (2x nocleh, 2x snídaně, 3x oběd, 2x večeře) <br><br> Všem párům TK Olymp soustředění vřele doporučujeme. Jedná se o tradiční, kvalitní a úspěšnou akci, kde si každý přijde na své. <br><br> Více informací najdete na tomto odkazu:http://www.dancesportpro.cz/klub/vanoce  <br><br><b> Přihlášky párů TK Olymp má na starosti Miroslav Hýža. Posílejte je na mail hyzam@tkolymp.cz ', 7, '0', '2010-12-04 23:00:00'),
(161, 1, 'Ze starých stránek', ' Je zapotřebí, aby si všechny páry na zítřejší společnou donesly tyč ze smetáku.(nejlépe dřevěnou)<br> Díky Ruda', 7, '0', '2010-12-05 23:00:00'),
(162, 1, 'Ze starých stránek', 'Páteční tréninky budou od 4h<br> Stupka ', 7, '0', '2010-12-08 23:00:00'),
(163, 1, 'Ze starých stránek', 'Sobotní tréninky proběhnou v době 10 - 13h<br> Nedělní tréninky upřesním zítra.<br> Informace mobil<br> Stupka', 7, '0', '2010-12-09 23:00:00'),
(164, 1, 'Ze starých stránek', 'Tréninky s Martinem Odstrčilem se v následujícím týdnu uskuteční v pátek. Časy společné hodiny a svých individuálních lekcí si zkontrolujte v sekci Rozpis tréninků.', 7, '0', '2010-12-11 23:00:00'),
(165, 1, 'Ze starých stránek', 'V úterý proběhne na Slovanském gymnáziu POVINNÝ volný trénink (volný = bez trenéra) od 16:30 do 18:30. Neomluvená neúčast na tomto tréninku znamená škrtnutí individuálních lekcí tento týden.', 7, '0', '2010-12-11 23:00:00'),
(166, 1, 'Ze starých stránek', 'Na pátečních trénincích s Martinem a Luckou proběhne ochutnávka cukroví. Aby bylo co ochutnávat, přineste všichni vzoreček napečeného cukroví. ;-)<br><br><b> Páteční tréninky budou poslední v roce 2010.', 7, '0', '2010-12-11 23:00:00'),
(167, 1, 'Ze starých stránek', '<b>DŮLEŽITÉ: <br></b><br>Přihlášky na Vánoční soustředění Prštice 2010 zasílejte na hyzam@tkolymp.cz <b>NEJPOZDĚJI DO ÚTERÝ 14.12. do 20:00. </b><br><br>  Dosud přihlášené páry:<br>  Stupka - Hlavicová<br> Hýža - Hýžová<br> Navrátil - Chytilová<br> Zárybnický - Dovčiaková<br> Špaňhel - Navrátilová<br> Šír - Šírová<br> Chytil - Slezáková<br> Pánek - Pánková<br> Stupka - Jendrulková', 7, '0', '2010-12-11 23:00:00'),
(168, 1, 'Ze starých stránek', 'Zkontrolujte si změněný rozpis lekcí Martina Odstrčila v pátek.', 7, '0', '2010-12-13 23:00:00'),
(169, 1, 'Ze starých stránek', 'V neděli od 11 do 13 h bude probíhat na SGO volný trénink, v pondělí potom od 17 do 19 h(taky na SGO).', 7, '0', '2010-12-17 23:00:00'),
(170, 1, 'Ze starých stránek', 'V uterý 21.12. bude probíhat na SGO od 17:00 do 19:00 volný trénink.', 7, '0', '2010-12-19 23:00:00'),
(171, 1, 'Ze starých stránek', '<b>Vystoupení tanečních párů v Hrubé Vodě<br><br></b>  páry, které budou vystupovat 22.12. v Hrubé Vodě, mají sraz v 19:30 v hotelu Akademic. Čas vystoupení je 19:45', 7, '0', '2010-12-19 23:00:00'),
(172, 1, 'Ze starých stránek', 'V úterý bude od 17:00 do 19:00 probíhat volný trénink PRO VŠECHNY PÁRY. Účast dobrovolná. ', 7, '0', '2010-12-19 23:00:00'),
(173, 1, 'Ze starých stránek', 'Soustředění Prštice <br><br> Abyste všichni zdárně dojeli na místo konání soustředění, podívejte se na následující odkaz. Adresa sokolovny je U Zámku 2, Prštice. Jeďte přes Ořechov.<br><br>http://www.mapy.cz/#mm=ZTtTcP@sa=s@st=s@ssq=pr%C5%A1tice%2C%20u%20z%C3%A1mku%202@sss=1@ssp=137837064_132529240_137849464_132539528@x=137843264@y=132533328@z=16', 7, '0', '2010-12-21 23:00:00'),
(174, 1, 'Ze starých stránek', '<b>Noví trenéři<br><br></b>  Od ledna 2011 u nás na několik měsíců končí s trénováním Kamila Limberková. Jako náhradu jsme na doporučení Ing. Jaroslava Kučery zvolili špičkový pár třídy M <b>Filip Karásek - Sabina Pišková</b> a nepravidelně i <b>Michala Kostovčíka</b> - bývalého mistra ČR a partnera Kamči.  <br><br> Na 3.1. byly vypsané nabídky lekcí, kde si můžete zarezervovat individuální hodiny s nimi. Lekce o délce 45 minut stojí 300 Kč/pár.', 7, '0', '2010-12-25 23:00:00'),
(175, 1, 'Ze starých stránek', '<b>První týden 2011<br><br></b>Tréninky s Filipem a Sabinou pro žlutou skupinu (viz předchozí upozornění) byly přeloženy na středu 5.1. Tréninky jsou podmíněné dostatečným zájmem o individuální lekce (alespoň 4 u každého trenéra). Lekce si můžete zapisovat nejpozději do 1.1. do 20:00.<br><br>V pondělí 3.1. proběhne volný trénink od 17 do 19 hodin na SGO - PRO VŠECHNY ČLENY ŽLUTÉ A ZELENÉ SKUPINY.<br><br> V úterý 4.1. proběhnou tréninky s Martinem Odstrčilem dle rozpisu.<br><br> Ve čtvrtek 6.1. proběhne pilates a practise jako obvykle (16:15 pilates, 17:30 practise)', 7, '0', '2010-12-29 23:00:00'),
(176, 1, 'Ze starých stránek', '<b>Workshop se Zdeňkem a Pavlou Landsfeldovými<br><br></b>  O víkendu 15-16.1. 2011 proběhne workshop se Zdeňkem a Pavlou Landsfeldovými. Základní balíček v ceně 800 Kč/pár zahrnuje 2 individuální a dvě společné lekce (vše po 60 minutách).  <br><br> Přihlašovat se můžete v sekci tréninková nabídka do 5.1. Účastníci workshopu si dále mohou od 6.1. zarezervovat s jednotlivými trenéry lekce navíc. ', 7, '0', '2010-12-29 23:00:00'),
(177, 1, 'Ze starých stránek', 'Dnes (v neděli) můžete jít na trénink na SGO od 14 h. ', 7, '0', '2011-01-01 23:00:00'),
(178, 1, 'Ze starých stránek', 'Příští týden proběhnou společné hodiny latiny v pondělí od 17 hodin.', 7, '0', '2011-01-05 23:00:00'),
(179, 1, 'Ze starých stránek', 'Nabídka tréninků s Filipem Karáskem a Sabinou Piškovou na pondělí bude aktivní do soboty 20:00. Zarezervujte si lekce (můžete i více než jednu) nejpozději do tohoto termínu. Na email hyzam@tkolymp.cz případně napište, jestli chcete spíše Filipa nebo Sabinu. Cena jedné lekce je 300 Kč.', 7, '0', '2011-01-05 23:00:00'),
(180, 1, 'Ze starých stránek', 'Pondělní volný trénink na SGO se nekoná z důvodu časové kolize s latinou na ZŠ Holečkova.', 7, '0', '2011-01-07 23:00:00'),
(181, 1, 'Ze starých stránek', 'V úterý na společné hodině s Martinem a ve čtvrtek na practise se bude platit Workshop s Pavlou a Zdeňkem Landsfeldovými.', 7, '0', '2011-01-08 23:00:00'),
(182, 1, 'Ze starých stránek', 'Páry, které mají zájem se účastnit pouze společných lekcí (netýká se těch, co mají zapsaný celý workshop) mohou na vededené hodiny (dle rozpisu) přijít, cena je 200 Kč/pár.', 7, '0', '2011-01-08 23:00:00'),
(183, 1, 'Ze starých stránek', 'Členské příspěvky na I. pololetí roku 2011 je nutné uhradit do 31.1.2011 na účet č. 1806875329/0800 nebo v hotovosti na společných lekcích.Výše členských příspěvků zůstává stejná.', 7, '0', '2011-01-08 23:00:00'),
(184, 1, 'Ze starých stránek', 'Do sekce tréninková nabídka napište, zda chcete indviduální lekce s Filipem nebo Sabinou pravidelně 1x týdně, případně jestli chcete pouze příští týden v pondělí. Cena lekce je 300 Kč/pár, aby přijeli oba trenéři, je nutné obsadit alespoň 8 lekcí.', 7, '0', '2011-01-10 23:00:00'),
(185, 1, 'Ze starých stránek', 'Pondělní společné hodiny budou rozdělené na dvě skupiny. Sabina Pišková povede skupinu párů hobby, D a C, na malé tělocvičně, Filip Karásek povede trénink skupiny párů tříd B a vyšší v baletním sále.<b> Tréninky začínají oproti zvyklosti v 18 hodin (viz rozpisy).', 7, '0', '2011-01-14 23:00:00'),
(186, 1, 'Ze starých stránek', 'Individuální lekce s Miroslavem Hýžou stojí od začátku roku 2011 240 Kč/60 min (180 Kč/45 min).', 7, '0', '2011-01-14 23:00:00'),
(187, 1, 'Ze starých stránek', 'V úterý 18.1. nebudou probíhat tréninky s Martinem Odstrčilem. Důvodem je příprava párů na mistrovství ČR ve standardních tancích, která probíhá v Pršticích. <br><br><b>Společná hodina bude nahrazena v pátek od 17 hodin na SGO s Miroslavem Hýžou.', 7, '0', '2011-01-15 23:00:00'),
(188, 1, 'Ze starých stránek', 'Společné hodiny latiny s Filipem a Sabinou budou příští týden ve středu.', 7, '0', '2011-01-21 23:00:00'),
(189, 1, 'Ze starých stránek', 'Cena individuálních lekcí s Terezou Jendrulkovou je od začátku roku 2011 200 Kč(za 45 min).', 7, '0', '2011-01-23 23:00:00'),
(190, 1, 'Ze starých stránek', 'Společné hodiny latiny s Filipem a Sabinou budou příští týden ve středu.', 7, '0', '2011-01-28 23:00:00'),
(191, 1, 'Ze starých stránek', 'V pátek 4.2.2011 se z důvodu pololetních prázdnin neuskuteční tréninky s Luckou Benýškovou ani jiné společné lekce.', 7, '0', '2011-01-31 23:00:00'),
(192, 1, 'Ze starých stránek', 'Příští týden se budou latinské tréninky konat v pondělí. Společná hodina DC bude od 17 hodin, společná hodina BAM bude od 18 hodin.<br><br>  Do sobotního večera si můžete zapisovat lekce navíc, poté budou vytvořeny rozpisy.', 7, '0', '2011-02-02 23:00:00'),
(193, 1, 'Ze starých stránek', 'O pololetních prázdninách, tj. zítra můžete jít na trénink od 10 do 12 hodin na baletní sál a od 13 do 15 hodin na Slovanské gymnázium. V sobotu potom můžete jít od 13 do 15 hodin na baletní sál.', 7, '0', '2011-02-02 23:00:00'),
(194, 1, 'Ze starých stránek', '<b>V neděli od 10:00 bude na SGO společná hodina s Ing. Zdeňkem Landsfeldem. Pro všechny členy žluté skupiny je zdarma, ostatní ji mohou navštívit za 100 Kč/pár.</b>', 7, '0', '2011-02-02 23:00:00'),
(195, 1, 'Ze starých stránek', 'Byly změněny dohodnuté termíny tréninků s Jardou Kučerou. Nejbližší termíny jsou 25.2., 14.3. a 17.3. (příprava na MČR v 10 tancích).', 7, '0', '2011-02-02 23:00:00'),
(196, 1, 'Ze starých stránek', 'Nabídka tréninků s Pavlou Landsfeldovou bude aktivní do úterý 21:00', 7, '0', '2011-02-02 23:00:00'),
(197, 1, 'Ze starých stránek', 'V úterý 15.2. budou mít páry Homola - Uvízlová a Anderle - Chromcová na baletním sále od 16:30 do 18:30 speciální přípravu před MČR v latinskoamerických tancích pod vedením Tomáše Krainy. Tréninky jsou hrazeny z peněz z vystoupení, na kterých tyto páry tančily.', 7, '0', '2011-02-03 23:00:00'),
(198, 1, 'Ze starých stránek', 'Latinské tréninky s Filipem a Sabinou budou příští týden ve středu. Obě společné lekce budou od 17:00', 7, '0', '2011-02-07 23:00:00'),
(199, 1, 'Ze starých stránek', 'Společná hodina s Pavlou v sobotu od 10:30 bude pro všechny členy Žluté skupiny zdarma.', 7, '0', '2011-02-10 23:00:00'),
(200, 1, 'Ze starých stránek', 'Na měsíc březen je jsou pozvaní trenéři Zdeněk Landsfeld a Pavla Landsfeldová. Pokud nebudou plně obsazeni (12 lekcí) páry našeho klubu do 25. (Zdeněk), resp. 28. (Pavla) února, budou tréninky zrušeny. Rezervace lekcí probíhá v sekci Tréninková nabídka.', 7, '0', '2011-02-10 23:00:00'),
(201, 1, 'Ze starých stránek', 'Z důvodu nemoci dnes odpadá cvičení pilates, místo něj bude probíhat volný trénink (od 16:15).', 7, '0', '2011-02-16 23:00:00'),
(202, 1, 'Ze starých stránek', 'Informace pro ČERVENOU SKUPINU:  <br><br> 1) Na poslední chvíli jsme se rozhodli udělat dětem víkendové soustředění. Chtěl bych se omluvit, že Vás informuji tak pozdě, ale vedly mě k tomu různé okolnosti. Tréninky budu učit já a se uskuteční v sobotu - tedy pozítří. Rozvrh bude následující: od 14 do 15 hodin bude společná hodina standardu, poté bude 45 min pauza a od 15:45 do 16:45 bude společná hodina latiny. Před, mezi a po společných hodinách budou probíhat individuální lekce. Cena za celý den je 200 Kč na pár (2x 60 min společných lekcí + 45 min individuální), soustředění bude částečně hradit klub - a to z peněz, které se vydělaly na vystoupeních. Pro jednotlivce bez partnerů (např. Simonka) bude soustředění bez individuálek za 50 Kč.Přijít samozřejmě mohou i ti, kteří se příští týden na soutěž nechystají. <br><br> 2) Ve stejný den večer - (sobota od 20:15) bude náš ples vystupovat na plese záchranné služby ve Slovanském domě. Budeme rádi za každý pár. <br><br> 3) Prosím všechny z Vás, kteří jste byli v letošním školním roce na vystoupeních, abyste mi napsali kolikrát a kde jste vystupovali (někteří už mi dávali lísteček na tréninku). Po uzavření plesové sezóny spočítáme vydělané peníze a podle účasti jednotlivých párů odměníme formou dotovaných tréninků všechny účastníky. <br><br> 4) Příští víkend pořádá KST Quick Olomouc soutěž Olomoucký taneční festival, kde rovněž mohou (a měli by) téměř všichni soutěžit (bohužel pořadatel nezařadil soutěž registrovaných dětí). Rovněž se můžete s dětmi přijít podívat na sobotní část do Slovanského domu, kde budou soutěžit starší páry z našeho klubu včetně nás trenérů a párů tříd A a M. V současné době je to jediná možnost, jak vidět v Olomouci soutěžit tyto páry. V nedělní části v Mariánském údolí by měly Vaše děti soutěžit.  <br><br> 5) Český svaz tanečního sportu pro letošní rok zavedl poplatky i pro páry HOBBY, poplatek je 200 Kč na tanečníka a je nutné ho uhradit dříve než na soutěž pojedete. Bez jeho zaplacení nelze soutěžit. Tuto částku buďto na svaz zašlete sami nebo jej přineste mně v sobotu, nejpozději v pondělí, abychom vše stihli zaplatit a vyřídit do soutěže. <br><br> Miroslav Hýža', 7, '0', '2011-02-16 23:00:00'),
(203, 1, 'Ze starých stránek', 'Upozorňujeme všechny HOBBY páry, že od nového roku vešlo v platnost pravidlo, že každý tanečník musí zaplatit příspěvek ČSTS ve výši 200 Kč. Bez zaplacení toho příspěvku NELZE SOUTĚŽIT. Více informací najdete na webu ČSTS nebo u členů klubové rady.', 7, '0', '2011-02-20 23:00:00'),
(204, 1, 'Ze starých stránek', 'Cena individuálních lekcí s Pavlem Johnem je od začátku roku 2011 160 Kč/60 min.', 7, '0', '2011-02-20 23:00:00'),
(205, 1, 'Ze starých stránek', 'V pátek se z důvodu ukázek a tréninků s Jardou Kučerou neuskuteční pravidelné individuální lekce s Miroslavem Hýžou', 7, '0', '2011-02-20 23:00:00'),
(206, 1, 'Ze starých stránek', '<b>Upozornění pro rodiče párů ČERVENÉ SKUPINY<br><br></b>  Do konce února je třeba zaplatit za Vaše děti členský příspěvek na druhé pololetí  ve výši 1500 Kč (stejně jako v prvním pololetí). Částku můžete zaplatit buďto převodem na účet č.: 1806875329-0800 nebo hotově trenérům na společných lekcích tento týden.', 7, '0', '2011-02-20 23:00:00'),
(207, 1, 'Ze starých stránek', 'O jarních prázdninách proběhnou tréninky s Martinem Odstrčilem ve větším objemu. Lekce nebudou rozepisovány podle pravidelných rozpisů, na úterý 1.3. byla vypsaná speciální nabídka. Zapisujte si nejvíce 2 lekce, pokud by se na někoho nedostalo, lekce budou přerozděleny podle klasických pravidel.', 7, '0', '2011-02-20 23:00:00'),
(208, 1, 'Ze starých stránek', 'Dva páry reprezentovaly TK Olymp na Mistrovství ČR v latinskoamerických tancích <br><br> V kategorii Junior I se Dominik Homola a Mirka Uvízlová umístili na 18-20. místě z 38 párů. <br><br> V kategorii Junior II se Jiří Anderle s Veronikou Chromcovou umístili na děleném 38. místě z 41 párů. <br><br> Všem gratulujeme a děkujeme za reprezentaci na vrcholné akci tanečního sportu.  Naši trenéři Filip Karásek se Sabinou Piškovou se umístili těsně před hranicí finále na 7-8. místě z 60 zúčastněných párů. Taktéž gratulujeme.', 7, '0', '2011-02-20 23:00:00'),
(209, 1, 'Ze starých stránek', 'Páteční společná hodina (od 17:30) s Jardou Kučerou je pro všechny členy ŽLUTÉ skupiny (LAT) zdarma. ', 7, '0', '2011-02-21 23:00:00'),
(210, 1, 'Ze starých stránek', 'Upozorňujeme všechny členy, že obvyklé tréninky se o jarních prázdninách konat nebudou a fungovat se bude v ,,jiném" režimu. Pokud máte zájem o individuální lekce s Martinem Odstrčilem, Filipem Karáskem nebo Sabinou Piškovou, zarezervujte si je ve speciálních tréninkových nabídkách.', 7, '0', '2011-02-24 23:00:00'),
(211, 1, 'Ze starých stránek', 'Tréninky s Martinem Odstrčilem se ruší. Společnou hodinu odučí Miroslav Hýža od 17:30 (přístupná červené a žluté skupině zdarma), tělocvična bude otevřená od 15:30. Teple se oblékněte, v tělocvičně se tento týden netopí, pouze se temperuje.', 7, '0', '2011-02-27 23:00:00'),
(212, 1, 'Ze starých stránek', 'Hodiny s Filipem a Sabinou si zapisujte nejpozději do dnešního odpoledne (do 18 hodin). Pokud nebude dostatečný počet lekcí, trenéři nebudou pozváni.', 7, '0', '2011-02-27 23:00:00'),
(213, 1, 'Ze starých stránek', 'Cena individuálních lekcí s Filipem a Sabinou je od měsíce března 350 Kč (45 min).', 7, '0', '2011-02-28 23:00:00'),
(214, 1, 'Ze starých stránek', 'Čtvrteční prektys a pilates se v době jarních prázdnin nekoná. Tento týden proběhnou pouze společná hodina STT s Miroslavem Hýžou a LAT s Filipem Karáskem a individuální lekce dle rozpisů. O víkendu proběhnou tréninky s Ing. Zdeňkem Landsfeldem.', 7, '0', '2011-02-28 23:00:00'),
(215, 1, 'Ze starých stránek', 'Tréninky se Zdeňkem Landsfeldem proběhnou pouze v neděli, viz rozpis. Společná lekce se tentokrát konat nebude. ', 7, '0', '2011-03-02 23:00:00'),
(216, 1, 'Ze starých stránek', 'Příští neděli od 10 hodin proběhne společná lekce STT s Pavlou Landsfeldovou pro celou žlutou skupinu ZDARMA.', 7, '0', '2011-03-02 23:00:00'),
(217, 1, 'Ze starých stránek', 'Do sobotního odpoledne (18 hodin) si můžete zapisovat lekce s Filipem a Sabinou nad rámec pravidelné nabídky. Společná lekce DC proběhne v pondělí od 17 a BAM od 18 hodin.', 7, '0', '2011-03-02 23:00:00'),
(218, 1, 'Ze starých stránek', 'Změněn rozpis lekcí s Martinem Odstrčilem, Sabinou Piškovou a Filipem Karáskem. Prosím překontrolujte si své lekce!!!', 7, '0', '2011-03-05 23:00:00'),
(219, 1, 'Ze starých stránek', 'Příští pondělí (14.3.) bude místo vedených latinských lekcí přednáška o IMAGE (líčení, česání, oblékání) s Filipem Karáskem a Sabinou Piškovou. ', 7, '0', '2011-03-06 23:00:00'),
(220, 1, 'Ze starých stránek', '<b>OBNOVENÍ PROVOZU </b><br><br> Během dnešního dne obnovíme všechny sekce stránek, které se opět rozbíhají pod novým webhostingem. ', 7, '0', '2011-03-18 23:00:00'),
(221, 1, 'Ze starých stránek', 'Tento týden proběhne cvičení pilates a následný prektys v tradičních časech ve čtvrtek od 16:15, resp. 17:30. ', 7, '0', '2011-03-18 23:00:00'),
(222, 1, 'Ze starých stránek', '<b>GRATULUJEME </b><br><br>Na dnešním Mistrovství České republiky v deseti tancích dosáhly oba naše páry obrovských úspěchů. Dominik Homola a Mirka Uvízlová byli v kategorii Junior I ve finále, kde obsadili 6. místo, Radim Stupka a Tereza Jendrulková vybojovali v kategorii dospělých 9-10. pozici. Oběma párům posíláme velkou gratulaci.', 7, '0', '2011-03-18 23:00:00'),
(223, 1, 'Ze starých stránek', 'V době, kdy stránky nebyly v provozu získali Radim Stupka s Terezou Jendrulkovou třídu M ve standardních tancích. Zpětně gratulujeme!', 7, '0', '2011-03-18 23:00:00'),
(224, 1, 'Ze starých stránek', '<b>TERMÍNY TRÉNINKŮ LATINY</b><br><br> V následujících týdnech budou probíhat tréninky latinskoamerických tanců v těchto dnech:<br><br>  pondělí 28.3. Filip a Sabina<br> pátek 1.4. s Jardou Kučerou<br> pondělí 4.4. s Filipem a Sabinou<br> pondělí 11.4. s Jardou Kučerou<br> pátek 15.4. s Filipem a Sabinou<br> středa 20.4. s Filipem a Sabinou<br><br>  Ve všech těchto dnech proběhnou také společné hodiny pro ŽLUTOU skupinu (podle rozpisů). Individuální lekce budou k dispozici v tréninkových nabídkách.  ', 7, '0', '2011-03-20 23:00:00'),
(225, 1, 'Ze starých stránek', '<b>HOBBY POPLATEK</b> <br><br> Upozorňujeme všechny členy HOBBY kategorie a rodiče mladších tanečníků této kategorie, že se blíží soutěže v Hlučíně, Kroměříži, Zlíně, Přáslavicích nebo Brně a většina členů stále nemá zaplacený poplatek, nutný ke startu na soutěžích.  Termíny a propozice soutěží najdete buďto na stránkách csts.cz nebo na nástěnce na Baletním sále. K uhrazení poplatku kontaktujte M. Hýžu na hyzam@tkolymp.cz <br><br> Zaplaceno mají:<br> Richard Pánek<br> Vilém Šír<br> Nela Šírová<br> Daniel Borůvka<br> Barbora Borůvková', 7, '0', '2011-03-22 23:00:00'),
(226, 1, 'Ze starých stránek', '<b>NA MISTRY SVĚTA SE SLEVOU</b><br><br>  Chtěli bychom všechny členy, kteří nebudou soutěžit na Brno Open 2011 pozvat na jedinečnou podívanou 25.-27. března do Brna. Bude se zde konat Brno Open 2011 zřejmě největší soutěž, jaká kdy v ČR proběhla. Momentálně je přihlášených 965 tanečních párů VČETNĚ DVANÁCTI MISTRŮ SVĚTA. Vzhledem k dobrým vztahům s pořádajícím klubem DSP Kometa Brno mají všichni členové TK Olymp Olomouc možnost navštívit tuto akci za vstupné 150 Kč/den místo 350 Kč. Více informací na www.brnoopen.cz', 7, '0', '2011-03-22 23:00:00'),
(227, 1, 'Ze starých stránek', '<b>ROZPISY PRAVIDELNÝCH TRÉNINKŮ<br><br></b> Počínaje příštím týdnem pište všechny připomínky, přání, stížnosti a omluvenky opět M. Hýžovi (737 545 525, hyzam@tkolymp.cz). V případě, že se nemůžete dostavit na individuální lekci, je vaší povinností omluvit se s dostatečným předstihem. Omluvenky v předvečer tréninku nebo v samotný den tréninku nemusí být vyřízeny a v případě nedostavení se na lekci bude klub po páru požadovat lekci v plné výši uhradit.  ', 7, '0', '2011-03-22 23:00:00'),
(228, 1, 'Ze starých stránek', 'Úterní společnou hodinu STT pro žlutou skupinu povede Adam Jendrulek', 7, '0', '2011-03-23 23:00:00'),
(229, 1, 'Ze starých stránek', 'Úterní společná hodina STT s Adamem Jendrulkem se pro nemoc ruší. Od 17:00 do 19:00 bude na SGO probíhat volný trénink.', 7, '0', '2011-03-27 22:00:00'),
(230, 1, 'Ze starých stránek', 'Prázdninové soustředění proběhne ve dnech 25.-30. srpna v Lošticích. Další informace budeme upřesňovat.', 7, '0', '2011-03-27 22:00:00'),
(231, 1, 'Ze starých stránek', '<b>Mistrovství České republiky družstev 2011 <br><br></b> Na Mistrovstvé ČR družstev, které se koná ve dnech 7.-8.5.2011 v Otrokovicích byly za TK OLYMP Olomouc nominovány tyto páry:<br><br> <b>kategorie Dospělí-STT</b><br> Radim Stupka - Tereza Jendrulková<br> Rudolf Stupka - Kristýna Hlavicová<br> Vít Navrátil - Petra Chytilová<br> Miroslav Hýža - Marie Hýžová<br> náhradníci:<br> Marek Švarc - Monika Vymětalová<br> Jakub Zárybnický - Kristýna Dovčiaková<br> <br> <b>kategorie Dospělí-LAT<br></b> Radim Stupka - Tereza Jendrulková<br> Miroslav Hýža - Marie Hýžová<br> Patrik Vymětal - Hana Brunclíková<br> Marek Švarc - Monika Vymětalová<br> náhradníci:<br> Jakub Zárybnický - Kristýna Dovčiaková<br> <br> O konečné podobě družstev rozhodne aktuální umístění na soutěžích.', 7, '0', '2011-03-30 22:00:00'),
(232, 1, 'Ze starých stránek', 'Ve dnech 29.4. - 1.5. se bude konat soustředění s Pavlou Landsfeldovou. Primárně bude pojato jako příprava na MČR družstev 2011, první 4 páry budou mít 3 hodiny hrazené klubem. ', 7, '0', '2011-03-30 22:00:00'),
(233, 1, 'Ze starých stránek', '<b>SHÁNÍME SPONZORY<br><br></b>Dnes v 18:30 na ZŠ Holečkova proběhne informační schůzka k organizaci soutěže Přáslavický pohár - Jarní cena TK Olymp Olomouc. Prosíme všechny členy a rodiče, kteří jsou schopní pomoci  ať už fyzickou účastí, finančně nebo mají nějaké kontakty na případné sponozory, aby přišli nebo se ozvali Miroslavu Hýžovi (tel: 737545525, mail: hyzam@tkolymp.cz). Čím víc peněz seženeme, tím lépe, případný zisk ze soutěže půjde na podporu prázdninového soustředění.', 7, '0', '2011-03-31 22:00:00'),
(234, 1, 'Ze starých stránek', 'V pátek se na Slovanském gymnáziu uskuteční tréninky s Filipem a Sabinou. Společná hodina bude pro všechny od 17 hodin. Individuální lekce navíc si můžete rezervovat do zítřka v tréninkové nabídce.', 7, '0', '2011-04-10 22:00:00'),
(235, 1, 'Ze starých stránek', '<b>Žádáme všechny členy, aby se pokusili sehnat materiální nebo finanční sponzorský dar na soutěž PŘÁSLAVICKÝ POHÁR - JARNÍ CENA TK OLYMP OLOMOUC. </b>Informace užitečné k oslovení sponzorů ve formátu PDF stáhnete zde: http://www.ulozto.cz/8614447/praslavice-sponzoring-pdf. Děkujeme.', 7, '0', '2011-04-10 22:00:00'),
(236, 1, 'Ze starých stránek', 'Uzávěrka rezervací lekcí s Pavlou Landsfeldovou bude 20. dubna. Tyto tréninky berte jako náhradu za Martina Odstrčila, který bude 26.4. a 3.5. trénovat převážně s páry účastnícími se MČR družstev.', 7, '0', '2011-04-11 22:00:00'),
(237, 1, 'Ze starých stránek', 'Dnes je na SGO od 17 hodin volný trénink. Společná hodina latiny bude ve středu od 17 hodin (jak zde již bylo psáno).', 7, '0', '2011-04-17 22:00:00'),
(238, 1, 'Ze starých stránek', 'Ve čtvrtek a pátek se nekonají společné hodiny (ani prektys a pilates). Oba dva dny bude možnost volného trénování a individuálních lekcí po dohod s trenéry.', 7, '0', '2011-04-18 22:00:00'),
(239, 1, 'Ze starých stránek', '<b>CHYSTÁNÍ SOUTĚŽE<br><br></b>  Žádáme všechny členy, aby přišli pomoct s chystáním nápisů a sálu na soutěž v tyto časy:<br><br>  <b>čtvrtek v 17 hod na Slovanském gymnáziu</b>: Bude se malovat velký nápis. S sebou vezměte štětce, palety, temperové barvy.  <br><br> <b>pátek v 18 hodin v Přáslavicích</b>: sraz 17:45 na Jiráskova 25, Olomouc, doprava auty - individuální - chystání stolů, židlí, výzdoby, šaten, atd...', 7, '0', '2011-04-19 22:00:00'),
(240, 1, 'Ze starých stránek', 'Ve čtvrtek bude telocvična SGO otevřená od 8 do 13 hodin, poté od 15 hodin.  <b>V 16:00 se pro zájemce koná zkrácený practise</b>, na který od 17 hodin navazuje malování nápisu na soutěž.<br><br>  V pátek bude telocvična SGO otevřená od 9 do 11 hod, poté od 14 do 16 hod.', 7, '0', '2011-04-19 22:00:00'),
(241, 1, 'Ze starých stránek', 'Všechny páry, které se budou účastnit MČR družstev 2011 v latinskoamerických tancích dostanou 28.4. 2 hrazené lekce s Jaroslavem Kučerou od klubu zdarma.', 7, '0', '2011-04-19 22:00:00'),
(242, 1, 'Ze starých stránek', 'Tréninky s Jardou Kučerou ve čtvrtek odpadají. Jarda odlétá na soutěž do Ruska o den dříve, než bylo v plánu.', 7, '0', '2011-04-24 22:00:00'),
(243, 1, 'Ze starých stránek', '<b>Latinský trénink tento týden<br><br></b>Společná hodina latiny bude tento týden ve středu od 17 hodin na ZŠ Holečkova pod vedením Miroslava Hýži.', 7, '0', '2011-04-24 22:00:00'),
(244, 1, 'Ze starých stránek', 'Marek Švarc s Monikou Vymětalovou a Kuba Zárybnický s Kristýnou Dovčiakovou dostanou příští týden několik lekcí s Filipem a Sabinou v rámci přípravy na MČR družstev zdarma.', 7, '0', '2011-04-24 22:00:00'),
(245, 1, 'Ze starých stránek', '<b>Tréninky latiny na příští týden<br><br></b>Příští týden budou tréninky s Filipem a Sabkou v pondělí i ve středu. Pravidelné lekce se budou konat ve středečním termínu, stejně jako společné lekce, které se uskuteční v 17:00. Na oba termíny si můžete v tréninkové nabídce rezervovat lekce navíc.', 7, '0', '2011-04-24 22:00:00'),
(246, 1, 'Ze starých stránek', '<b>Poděkování<br><br></b>  Děkujeme VŠEM členům, rodičům i prarodičům za pomoc při organizaci soutěže, která se díky opravdu velkému týmu povedla na výbornou.', 7, '0', '2011-04-24 22:00:00'),
(247, 1, 'Ze starých stránek', 'Radkovi Vymazalovi s Míšou Ochmanovou gratulujeme k zisku třídy C jak ve standardních, tak v latinskoamerických tancích.', 7, '0', '2011-04-24 22:00:00'),
(248, 1, 'Ze starých stránek', 'V neděli od 10 hodin bude pro celou Žlutou skupinu společná hodina standardních tanců s Pavlou Landsfeldovou zdarma. Všichni, kteří jste si zarezervovali lekce, si zkontrolujte časy v rozpisech.', 7, '0', '2011-04-27 22:00:00'),
(249, 1, 'Ze starých stránek', 'Nové nášivky na fraky, vesty, kalhoty či košile s nápisem ,,OLYMP TEAM" jsou k dispozici u Miroslava Hýži. Cena 1 ks je 40 Kč.', 7, '0', '2011-05-05 22:00:00'),
(250, 1, 'Ze starých stránek', 'Latinské tréninky s Filipem a Sabinou budou v květnu každé pondělí. 19. května bude společná hodina LAT s Jardou Kučerou.', 7, '0', '2011-05-08 22:00:00'),
(251, 1, 'Ze starých stránek', 'V tréninkové nabídce máte možnost rezervace lekcí s Markem Dvorníkem. Marek je Mistr ČR v latinskoamerických tancích kategorie profesionálů a výborný trenér LAT. Lekce s ním stojí 500 Kč.', 7, '0', '2011-05-09 22:00:00'),
(252, 1, 'Ze starých stránek', '<b> Prázdninové soustředění LOŠTICE 2011<br><br></b>   V termínu 25.-30. srpna se uskuteční klubové soustředění v Lošticích. V sekci Tábory a soustředění se můžete přihlašovat. Všechny informace  najdete ZDE: http://www.ulozto.cz/8988109/lostice-2011-zip', 7, '0', '2011-05-12 22:00:00'),
(253, 1, 'Ze starých stránek', '<b> OLYMPÁCKÝ ŽEBŘÍČEK<br><br></b>  Znovu po roce je tu klubová soutěž Olympácký žebříček 2011. Uskuteční se v neděli 22.5. 2011 v KD Sidia v Olomouci. Akce je přístupná všem rodičům, příbuzným i kamarádům. Přesný harmonogram, soutěžní kategorie a další informace budou v následujících dnech upřesněny zde na stránkách.<br><br> Účast na akci je pro všechny členy klubu povinná. V rámci soutěže proběhne i celoklubové focení.', 7, '0', '2011-05-12 22:00:00'),
(254, 1, 'Ze starých stránek', 'Pondělní společné hodiny s Filipem a Sabinou budou do konce roku probíhat v čase od 18 hodin.', 7, '0', '2011-05-13 22:00:00'),
(255, 1, 'Ze starých stránek', 'Pravidelné individuální lekce s Miroslavem Hýžou se do konce května z důvodu zkouškového období ruší.', 7, '0', '2011-05-13 22:00:00'),
(256, 1, 'Ze starých stránek', '<b>VALNÁ HROMADA OLOMOUCKÉ DIVIZE ČSTS<br><br></b>  Dne 24.5. 2011 od 16 h se v prostorách DDM na tř. 17. listopadu uskuteční Valná hromada členů Olomoucké divize Českého svazu tanečního sportu. Bude se volit předseda a nové předsednictvo Jakožto nejpočetnější klub bychom měl mít náš hlas také největší váhu. Proto prosíme všechny členy, kteří se nechystají aktivně účastnit, aby vyplnili plnou moc a podepsanou ji přinesli na trénink ještě <b>TENTO TÝDEN.</b>Za členy mladší 18 let se valné hromady účastní (nebo plnou moc podepisuje) jeden z rodičů. <br><br> Formulář plné moci zde: http://www.ulozto.cz/9021853/plna-moc-valna-hromada-doc<br><br> Heslo pro stažení: olymp   		', 7, '0', '2011-05-15 22:00:00'),
(257, 1, 'Ze starých stránek', '<b>OLYMPÁCKÝ ŽEBŘÍČEK<br><br></b>  Prezence: v neděli 22.5. ve 14 hodin v KD Sidia  <br><br> Kategorie:<br> 1. soutěž: děti a junioři (do 15 let včetně)<br> 2. soutěž: mládež a dospělí (od 16 let)<br> - děti budou tančit společně s juniory, na konci soutěže budou vyhlášeny i nejlepší dětské páry (do 11 let včetně)<br><br>  Soutěž proběhne zároveň ve STT a LAT tancích, účastní se i ti, co tančí pouze jednu skupinu tanců.<br><br>', 7, '0', '2011-05-15 22:00:00'),
(258, 1, 'Ze starých stránek', 'Kvůli valné hromadě OD neproběhnou příští týden tréninky s Martinem Odstrčilem.', 7, '0', '2011-05-15 22:00:00'),
(259, 1, 'Ze starých stránek', 'Nezapomeňte se hlásit na soustředění do Loštic (viz sekce Tábory a soustředění). Kvůli velkému zájmu byla kapacita posunuta na 60 míst. Momentálně je ještě 10 volných.', 7, '0', '2011-05-16 22:00:00'),
(260, 1, 'Ze starých stránek', 'Společná hodina s Jardou Kučerou se tentokrát nekoná. Normálně proběhne pilates a practise.', 7, '0', '2011-05-17 22:00:00'),
(261, 1, 'Ze starých stránek', 'Výsledky klubového žebříčku a fotky uveřejníme v průběhu týdne.', 7, '0', '2011-05-22 22:00:00'),
(262, 1, 'Ze starých stránek', '<b>Tréninky latiny na zbytek května a celý červen pro žlutou skupinu:<br><br></b> pondělí 30.5. společná DC od 17 hod<br> středa 1.6. společná BAM od 17 hod<br> středa 8.6. společné DC a BAM od 17 hod<br> pondělí 13.6. společné DC a BAM od 18 hod<br> pondělí 20.6. společné DC a BAM od 18 hod<br> <br> Vše proběhne na ZŠ Holečkova.', 7, '0', '2011-05-24 22:00:00'),
(263, 1, 'Ze starých stránek', '<b>Olympácký žebříček 22.5. 2011 - KD Sidia<br><br></b>  junioři:<br> 1. Matěj Očenášek - Marie Smyslilová<br> 2. Jiří Anderle - Veronika Chromcová<br> 3. Marek Chytil - Petra Slezáková<br> <br> Děti:<br> 1. Marek Chytil - Petra Slezáková<br> 2. Matěj Beneš - Kristýna Freyová<br> 3. Vilém Šír - Nela Šírová<br> <br> Dospělí:<br> 1. Miroslav Hýža - Marie Hýžová<br> 2. Jakub Zarybnicky - Kristýna Dovčiaková<br> 3. Vít Navrátil - Petra Chytilová<br><br>  Kompletní výsledky dětí a juniorů najdete zde (heslo pro stažení je: olymp): http://www.ulozto.cz/9125525/zebricek-2011-pdf <br><br> Kompletní výsledky dospělých párů najdete zde (heslo pro stažení je: olymp): http://www.ulozto.cz/9126708/zebricek-dospeli-2011-pdf', 7, '0', '2011-05-25 22:00:00'),
(264, 1, 'Ze starých stránek', 'Oproti původnímu plánu byly přehozeny společné lekce latiny skupin DC a BAM. Skupina DC bude mít společnou lekci hned v pondělí (17:00 na ZŠ Holečkova), aby páry mohly přijít i na společnou lekci ve středu na SGO s Adamem Jendrulkem od 18 hod. Společná lekce BAM s Filipem Karáskem proběhne ve středu od 17 hod na baletním sále.', 7, '0', '2011-05-28 22:00:00'),
(265, 1, 'Ze starých stránek', '<b>Platby letního soustředění<br><br></b>  Termín zaplacení letního soustředění byl posunut - do 31.7. Platby provádějte převodem na účet TK Olymp. Cena soustředění pro registrované děti, hobby děti a hobby juniory je 3900 Kč/osobu, pro registrované juniory, mládež a dospělé je cena 4200 Kč na osobu. <br><br> číslo účtu: 1806875329/0800<br> variabilní symbol: rodné číslo<br> do poznámky vepište jméno člena  <br><br><b>Informace k soustředění jsou ke stažení zde: <br></b> http://www.ulozto.cz/9173675/dopis-pdf', 7, '0', '2011-05-29 22:00:00'),
(266, 1, 'Ze starých stránek', 'Fotky z Olympáckého žebříčku 2011 najdete zde: http://tanecni-olomouc.cz/media/foto/scripts/thumbs2.php?dir=2011/odpoledne%20progress%20olymp', 7, '0', '2011-05-30 22:00:00'),
(267, 1, 'Ze starých stránek', 'V pondělí bude pro všechny členy žluté skupiny volný trénink na SGO od 17 do 19 hodin.', 7, '0', '2011-06-03 22:00:00'),
(268, 1, 'Ze starých stránek', '<b>Budeme pořádat ÁČKA <br><br></b>S radostí Vám oznamujeme, že TK Olymp Olomouc dostal ve výběrovém řízení pro rok 2012 přidělené pořádání soutěže třídy A. O tuto soutěž jsme zažádali poprvé v historii klubu, proto její přidělení můžeme považovat za velký úspěch.', 7, '0', '2011-06-04 22:00:00'),
(269, 1, 'Ze starých stránek', 'Tréninky s Martinem Odstrčilem se pro tento týden ruší (osobní důvody). Společnou lekci povede ve stejném čase (17:30-18:30) Miroslav Hýža.', 7, '0', '2011-06-05 22:00:00'),
(270, 1, 'Ze starých stránek', '<b>ZAKONČENÍ TANEČNÍ SEZÓNY 2010/2011 <br><br></b>  Dne 24.6. od 16 hod proběhne formou zkráceného prektysu a ukázek zakončení taneční sezóny. Sraz pro tanečníky je v 15:30. Zváni jsou všichni členové, trenéři, rodiče, bývalí členové, atd.  <br><br> Po tanečních vystoupeních proběhne tradiční raut ve stylu: co kdo upeče, nachystá, koupí, atd... Bylo by dobré, kdyby se každý, kdo se jej chce účastnit, přičinil o jeho pestrost. <br><br> Večer bude volně navazovat Salsa party TŠ Progress v U-klubu.', 7, '0', '2011-06-08 22:00:00'),
(271, 1, 'Ze starých stránek', '<b>PŘIHLÁŠKA NA SOUSTŘEDĚNÍ LOŠTICE 2011 <br><br></b>  V níže přiloženém odkazu je ke stažení přihláška do Loštic. Vyplněnou i s potvrzením od lékaře (mladší 18 let) ji odevzdejte do konce června Mirkovi. Platby provádějte převodem na účet klubu (info na přihlášce) do konce července.<br><br> http://www.ulozto.cz/9364447/prihlaska-lostice-pdf<br><br> Heslo: tvaruzky', 7, '0', '2011-06-12 22:00:00'),
(272, 1, 'Ze starých stránek', '<b>ATLETICKÁ PŘIPRAVA<br><br></b>  Na podnět trenérů Martina Odstrčila a Jardy Kučery jsme pro Vás na letošní léto připravili novinku. Počínaje pondělkem 27.6. začnou atletické tréninky s profesionálními trenéry Atletického klubu Olomouc, s kterými máme výborné zkušenosti z individuálních lekcí. <br><br> Tréninky budou probíhat 4 týdny každé pondělí a středu od 18:00 do 19:30 na atletickém stadionu na tř. 17. listopadu.  <br><br> Cena všech tréninků je 350 Kč na osobu, v případě účasti celého páru, dostane slevu 300 Kč na libovolnou individuální lekci na soustředění v Lošticích. <br><br> Tréninky jsou určené dospělým, mládežníkům a juniorům od 13 roků. ', 7, '0', '2011-06-12 22:00:00'),
(273, 1, 'Ze starých stránek', '<b>LOŠTICE - individuální lekce<br><br></b>  Individuální lekce na soustředění do Loštic hlaste elektronickou poštou na hyzam@tkolymp.cz do konce června. Po tomto datu bude možné zarezervovat si lekce pouze v případě, že se do deadlinu všechny neobsadí.<br><br>Na soustředění bude možné trénovat s těmito trenéry: Odstrčil, Kučera, Landsfeldová, Karásek, Pišková, Limberková, Jendrulková, Jendrulek, John, Benýšková a Hýža.', 7, '0', '2011-06-12 22:00:00'),
(274, 1, 'Ze starých stránek', 'Úterní individuální tréninky s Martinem Odstrčilem se ruší, společnou hodinu povede v čase 17:30-18:30 Miroslav Hýža.', 7, '0', '2011-06-18 22:00:00'),
(275, 1, 'Ze starých stránek', 'Ve čtvrtek neproběhne ani practise ani pilates. V čase 16-20 h proběhne na SGO volný trénink. Páteční společné hodiny rovněž neproběhnou kvůli časové kolizi se Zakončením taneční sezóny (info níže)', 7, '0', '2011-06-19 22:00:00'),
(276, 1, 'Ze starých stránek', '<b>Červencová příprava <br><br></b> Společně s ostatními zkušenějšími tanečníky jsme si naplánovali první 4 týdny přípravy na novou sezónu. Jako u jiných sportů bude zaměřené převážně na kondici a fyzickou připravenost. Zúčastnit se jí mohou všichni členové, kteří budou mít zájem. Jediné hodiny, které se platí, jsou atletické tréninky (informace o placení viz níže). Úterní a čtvrteční tréninky proběhnou v tělocvičně SGO. <br><br> Mirek Hýža <br><br> <b>Program - týden první:</b> <br> pondělí 27.6. 18:00 - 19:30 atletika<br> úterý 28.6. 18:30 - 19:30 - competition practise<br> středa 29.6. 18:00 - 19:30 - atletika<br> čtvrtek 30.6. 18:30 - 19:30 - basic Radim<br> pátek 1.7. 9:00 - plavání (sraz 8:45 před plaveckým stadionem)', 7, '0', '2011-06-26 22:00:00'),
(277, 1, 'Ze starých stránek', 'Zítra proběhne na SGO od 14 hod do začátku prektysu (18:30) volný trénink.', 7, '0', '2011-06-26 22:00:00'),
(278, 1, 'Ze starých stránek', 'v pátek od 18 h proběhne atletický trénink namísto pondělí 4.7.', 7, '0', '2011-06-28 22:00:00'),
(279, 1, 'Ze starých stránek', 'Ve čtvrtek bude tělocvična otevřená od 8 do 10 h, potom od 15:30 až do večera. Od 18:30 proběhne latinský vedený trénink s Radimem Stupkou.', 7, '0', '2011-06-28 22:00:00'),
(280, 1, 'Ze starých stránek', '<b>ČERVENCOVÁ PŘÍPRAVA -TÝDEN DRUHÝ <br><br></b>  pondělí 4.7. 18:00 - 19:30  - volný trénink <br> úterý 5.7. 18:30 - 19:30 - practise<br> středa 6.7. 18:00 - 19:30 - atletika<br> čtvrtek 7.7. 18:30 - 19:30 - vedená lekce s Terezou Jendrulkovou<br> pátek 8.7. 9:00 - plavání na plaveckém stadionu - v osm hodin se sejdeme na Facebooku a poradíme se podle počasí <br><br> Volné tréninky budou probíhat po dohodě na Facebooku. Kdo máte zájem si přijít potrénovat, ozvěte se s dostatečným předstihem do skupiny Informační kanál TK Olymp.', 7, '0', '2011-07-03 22:00:00'),
(281, 1, 'Ze starých stránek', '<b>ČERVENCOVÁ PŘÍPRAVA - TÝDEN TŘETÍ <br><br></b> pondělí 11.7. <br>  - 9:00 - 11:00 - volný trénink<br>  - 16:00 - 17:30 - volný trénink <br>  - 18:00 - 19:30 - atletika<br> úterý 12.7. <br>  - 18:30 - 19:30 - practise<br> středa 13.7. <br>  - 9:00 - 11:00 - volný trénink<br>  - 16:00 - 17:30 - volný trénink <br>  - 18:00 - 19:30 - atletika<br>  čtvrtek 14.7. <br>  - 9:00 - 11:00 - volný trénink<br>  - 18:30 - 19:30 - vedená lekce s Mirkem Hýžou<br>  pátek 15.7. <br>   - 9:00 - plavání na plaveckém stadionu - v osm hodin se sejdeme na <br>Facebooku a poradíme se podle počasí  <br><br>neděle 17.7. 11:00 BOWLING - info na Facebooku', 7, '0', '2011-07-09 22:00:00'),
(282, 1, 'Ze starých stránek', '1.-5. srpna si nebude k dispozici tělocvična SGO z důvodu malování.', 7, '0', '2011-07-13 22:00:00'),
(283, 1, 'Ze starých stránek', 'Dnešní atletika se přesouvá na čtvrtek. Dave volal, že má zdravotní problémy a dnes dojít nemůže. Náhradní termín = čtvrtek 18:00.   Prosím dejte si to navzájem vědět. M.', 7, '0', '2011-07-17 22:00:00'),
(284, 1, 'Ze starých stránek', 'V sobotu od 18:30 proběhne na SGO společná hodina STT s Adamem Jendrulkem! ', 7, '0', '2011-07-20 22:00:00'),
(285, 1, 'Ze starých stránek', '<b>ČERSTVÉ INFORMACE OHLEDNĚ TĚLOCVIČNY:<br><br></b>  S trénováním můžeme začít již dnes. Pro všechny, kdo chtějí přjít dnes: otevřeno bude od 16 hodin <br><br> Dále pro tento týden platí, že trénovat se z důvodu úklidu smí pouze odpoledne (od 14 hod), po každém tréninku bude vytřená tělocvična a vchod nebude přes dvojité dveře, ale přímo do nářaďovny (tam kde je skříň s hudbou). Šatnu nevyužívejte, převlékejte se přímo v tělocvičně. <br> <br>Každý, kdo přijde trénovat je povinen se zapsat na papír, který bude ve skříni s hudbou.', 7, '0', '2011-08-09 22:00:00'),
(286, 1, 'Ze starých stránek', '<b>LOŠTICE JSOU ZA DVEŘMI<br><br></b>Blíží se taneční soustředění Loštice 2011. Všechny informace jste měli možnost si stáhnout nebo vyzvednout. A protože opakování je matka moudrosti, zde jsou ty nejdůležitější:<br><br>  <b>termín:</b> 25.-30.8. 2011<br>  - sraz ve čtvrtek v 8 hodin na hotelu, konec v úterý v 18:30 hodin (od 17 hodin začne závěrečný practise pro rodiče) <br><br> <b>místo:</b> Hotel Střelnice, Havelkova 16, Loštice<br> GPS: 49°44''24.01”N 16°55''47.46”E <br><br> <b>doprava</b>: individuální <br><br> odevzdání přihlášek a zaplacení již proběhlo <br><br> <b>platba individuálních lekcí:</b> při příjezdu na místě (mějte připraveny přesné částky)<br><br> Těšíme se na Vás!', 7, '0', '2011-08-19 22:00:00'),
(287, 1, 'Ze starých stránek', 'Na tomto odkazu si můžete stáhnout platby za individuální lekce, které se budou vybírat zítra při nástupu: http://www.ulozto.cz/10063367/platby-zlekce-pdf', 7, '0', '2011-08-23 22:00:00'),
(288, 1, 'Ze starých stránek', 'Během víkendu zde budou umístěny informace ohledně členské schůze a tréninků na příští týden.', 7, '0', '2011-09-01 22:00:00'),
(289, 1, 'Ze starých stránek', '<b>Tréninky na první záříjový týden:</b><br><br>  ŽLUTÁ SKUPINA (registrovaní junioři a dospělí)<br><br>  V pondělí proběhne vedená lekce LAT na ZŠ Holečkova v 18:00, v úterý vedená lekce STT od 17:30 na SGO. Ve čtvrtek proběhne cvičení pilates od 16:15 a practise od 17:30 na SGO.  <br><br> ČERVENÁ SKUPINA (děti a neregistrovaní junioři) <br><br> pondělí 17:00 společná hodina STT (ZŠ Holečkova)<br> čtvrtek 16:15 pilates, 17:30 practise (Slovanské gymnázium)<br> pátek 17:00 společná hodina LAT (ZŠ Holečkova)<br>', 7, '0', '2011-09-03 22:00:00'),
(290, 1, 'Ze starých stránek', '<b>ČLENSKÁ SCHŮZE</b><br><br>  KDY: čtvrtek 15.9. 2011 17:00 (neplést s tímto čtvrtkem)<br><br>  KDE: ZŠ Holečkova, Malá tělocvična <br><br> Příští čtvrtek proběhne na ZŠ Holečkova členská schůze občanského sdružení Taneční klub Olymp Olomouc. Doporučujeme všem členům nebo v případě nedovršení 18 let jejich zákonným zástupcům, účastnit se této schůze, jelikož všichni členové jsou povinní dodržovat rozhodnutí z této schůze. Náplní bude jako vždy shrnutí činnosti a hospodaření klubu za uplynulý školní rok a představení plánu na rok příští. Budou se řešit tréninky, individuální lekce a v neposlední řadě se budou vybírat členské příspěvky na toto pololetí. V případě nezaplacení členských příspěvků nebude možné trénovat v tréninkových prostorách TK Olymp. Výše příspěvků pro žlutou skupinu zůstává z minulého pololetí, pro červenou skupinu budou o 200 Kč dražší z důvodu zvýšení tréninkových dávek.<br><br>  ČLENSKÉ PŘÍSPĚVKY:<br> <b>Žlutá skupina:</b> 2500 Kč, (pouze STT nebo LAT: 1700 Kč)<br>  - trenéři: Odstrčil, Karásek, Pišková, Limberková, Kučera, Landsfeld, Landsfeldová<br> <b>Červená skupina:</b> 1700 Kč<br>  - trenéři: Benýšková, Jendrulková, Hýža <br><br> <b>BURZA TANEČNÍ OBUVI A KOSTÝMŮ:</b><br><br> KDY: čtvrtek 15.9. 2011 v 16:15<br> KDE: ZŠ Holečkova, Malá tělocvična     ', 7, '0', '2011-09-03 22:00:00'),
(291, 1, 'Ze starých stránek', 'Cena individuálních lekcí Lucky Benýškové je od nového školního roku 160 Kč/60 min a 120 Kč/45 min.', 7, '0', '2011-09-03 22:00:00'),
(292, 1, 'Ze starých stránek', '<b>Tréninky na tento týden:</b><br><br>  ŽLUTÁ SKUPINA<br> pondělí: volný trénink na baletním sále od 17 do 20 h<br> úterý: společná hodina s Martinem Odstrčilem od 17:30, volný trénink od 16 do 21 h (mimo vedenou hodinu)<br> středa: společná hodina s Kamilou Limberkovou od 17 hodin na ZŠ Holečkova, volný trénink na baletním sále od 15:30 do 19:30 (mimo vedenou hodinu)<br> čtvrtek: ČLENSKÁ SCHŮZE NA ZŠ Holečkova (info níže)<br> pátek: volný trénink na SGO od 16 do 20 h.<br><br>  ČERVENÁ SKUPINA<br> pondělí: vedená hodina na ZŠ Holečkova od 17:00<br> čtvrtek: ČLENSKÁ SCHŮZE<br> pátek: vedená hodina od 17:00<br><br>  Individuální lekce domlouvejte buďto přes Miroslava Hýžu na hyzam@tkolymp.cz nebo rovnou s trenéry. Nechoďte na sály mimo vymezenou dobu. ', 7, '0', '2011-09-10 22:00:00'),
(293, 1, 'Ze starých stránek', 'Středeční tréninky s Kamilou Limberkovou se ruší. Náhradní termín bude v neděli, kdy budou nahrazeny lekce ze soustředění v Lošticích. Středeční společná lekce bude ve stejném čase s Terkou Jendrulkovou.', 7, '0', '2011-09-12 22:00:00'),
(294, 1, 'Ze starých stránek', '<b>Individuální lekce</b><br><br>  Do sekce Tréninková nabídka byly vloženy nabídky PRAVIDELNÝCH tréninků s Martinem Odstrčilem (1x týdně: úterý), Filipem Karáskem a Sabinou Piškovou (1x za 14 dní: pondělí), Kamilou Limberkovou (1x za 14 dní: středa) a Miroslavem Hýžou (1x týdně: po-pá dle domluvy). Požadavky na čas tréninku (pokud nějaké máte), napište na hyzam@tkolymp.cz. <br><br> Pokud máte zájem o pravidelné individuální lekce s dalšími trenéry (Lucka Benýšková, Tereza Jendrulková, Radim Stupka, Rudolf Stupka, Kristýna Hlavicová, Adam Jendrulek, Vít Navrátil, Petra Chytilová, Pavel John), napište své požadavky na hyzam@tkolymp.cz. <br><br> Ceník individuálních lekcí: <br><br> Jarda Kučera 800 Kč/45 min<br> Martin Odstrčil 600 Kč/45 min<br> Kamila Limberková 400/45 min<br> Filip Karásek 350/45 min<br> Sabina Pišková 350/45 min<br> Tereza Jendrulková 200/45 min<br> Miroslav Hýža 200/45 min<br> Lucie Benýšková 160/60 min<br> Pavel John 160/60 min<br> ostatní 100 Kč/60 min', 7, '0', '2011-09-15 22:00:00'),
(295, 1, 'Ze starých stránek', '<b>NÁBOR NOVÝCH ČLENŮ - možnost vydělat si na individuálky!</b><br><br>   Dne 3. října se bude konat informační schůzka k náboru nových členů do našeho klubu. Proběhne na ZŠ Holečkova od 18 h.  <br><br> Nabíráme členy napříč věkovými kategoriemi (od dětí po dospělé), cena kurzů pro začátečníky a mírně pokročilé je 1500 Kč na osobu/pololetí.  <br><br> Pro všechny členy našeho klubu (resp. rodiče) máme nabídku, která již zazněla na členské schůzi: <b>Přiveďte, své známé, kamarády atd. Za každý pár, který se zapíše a zaplatí zápisné dostanete 1000 Kč na individuální lekce. </b> <br><br> Rozesílejte e-maily, zkuste jít do tříd na svých současných nebo bývalých školách, využijte Facebook a jiné sociální sítě, či vymyslete jiný způsob. Je to na Vás!<br><br> Pokud by chtěl někdo pomoci s informačním letáčkem, které by poté rozdával ve školách či jinde, napište na hyzam@tkolymp.cz a domluvíme se.', 7, '0', '2011-09-19 22:00:00'),
(296, 1, 'Ze starých stránek', '<b>Volné tréninky na Slovanském gymnáziu:<br><br></b>  Volné tréninky budou v tělocvičně SGO probíhat v těchto hodinách:<br> pondělí: od 17:00<br> úterý: od 16:00 do 17:30 a od 18:30 (mimo společnou hodinu pořád)<br> středa: od 17:00<br> čtvrtek: od 19:00<br> pátek: od 15:15<br> sobota, neděle, svátky: dle domluvy ve skupině Informační kanál TK Olymp na Facebooku (o přidání do skupiny zažádejte M. Hýžu) <br><br> <b>Upozornění pro všechny:<br><br> </b>Mimo tyto časy není možné chodit trénovat. Každý trénink je nutné zapsat do prezenční listiny a ohlásit na Facebooku do IK TK Olymp. Člen, který končí trénink a zamyká je zodpovědný za úklid, zhasnutí, zavřená okna a zamknutí. Jakékoliv porušení pravidel může znamenat až vyloučení z klubu. Dávejte si prosím pozor!', 7, '0', '2011-09-19 22:00:00'),
(297, 1, 'Ze starých stránek', '<b>Taneční soutěže pořádané Olympem v následujícím kalendářním roce: <br></b><br> 7.-8.4. 2012: Přáslavický pohár - Jarní cena TK Olymp Olomouc, Přáslavice<br> 3.11. 2012: Memoriál Dr. Miroslava Hýži, Regionální centrum, Olomouc', 7, '0', '2011-09-19 22:00:00'),
(298, 1, 'Ze starých stránek', 'Středeční společná hodina fialové skupiny se přesouvá na 17:00. Změna byla vynucená školním rozvrhem trenérky Terezy Jendrulkové, který dostala k dispozici až tento týden.', 7, '0', '2011-09-21 22:00:00'),
(299, 1, 'Ze starých stránek', 'Radim Stupka a Tereza Jendrulková zakončili své soutěžení v třídě A, latinskoamerických tancích, suverénním prvním místem a získali tak třídu M, gratulujeme!!!', 7, '0', '2011-09-24 22:00:00'),
(300, 1, 'Ze starých stránek', 'Ve středu 17. listopadu proběhnou i přes státní svátek společné lekce s Kamilou Limberkovou (18:00 ŽLUTÁ) a Terezou Jendrulkovou (17:00 FIALOVÁ).', 7, '0', '2011-09-24 22:00:00'),
(301, 1, 'Ze starých stránek', 'V pondělí bude pro ŽLUTOU skupinu od 17:00 volný trénink jak na SGO, tak na ZŠ Holečkova.', 7, '0', '2011-09-24 22:00:00'),
(302, 1, 'Ze starých stránek', 'Ve čtvrtek výjimečně neproběhne pilates a practise. Žlutá skupina bude mít vedenou lekci s Jardou Kučerou na ZŠ Holečkova od 17:30.', 7, '0', '2011-09-24 22:00:00'),
(303, 1, 'Ze starých stránek', 'V pátek 30.9. v 19:00 na SGO proběhne pro zájemce practise.', 7, '0', '2011-09-26 22:00:00'),
(304, 1, 'Ze starých stránek', 'Ze záhadných technických důvodů (patrně chyba ve zdrojovém kódu stránky) přes noc zmizely rozpisy s Filipem Karáskem a Sabinou Piškovou. TRÉNINKY PROBĚHNOU PODLE PŮVODNÍHO PLÁNU!!!', 7, '0', '2011-10-02 22:00:00'),
(305, 1, 'Ze starých stránek', '<b>GRAND SLAM Czech Dance Open 2011 Ostrava <br></b><br> Na jedné z nejlepších soutěží, které se kdy v ČR konaly, startovaly dva olympácké páry. Oba se probojovaly do druhého kola Grand Slamu v latinskoamerických tancích. <br><br> Radim Stupka a Tereza Jendrulková 65.<br> Miroslav Hýža a Marie Hýžová 72.<br><br>  Oba páry a hlavně téměř kompletní světovou špičku můžete vidět v televizním vysílání, které běželo v přímém přenosu na ČT 4: <br><br> http://www.ceskatelevize.cz/ivysilani/10171877043-tanec/411232400011043-czech-dance-open-2011-ostrava/', 7, '0', '2011-10-02 22:00:00'),
(306, 1, 'Ze starých stránek', 'V pátek od 17:30 proběhne na Baletním sále společná lekce latiny s Jardou Kučerou.', 7, '0', '2011-10-04 22:00:00'),
(307, 1, 'Ze starých stránek', 'Dnes odpadá pilates, practise je jako obvykle od 17:30. Tělocvična bude přístupná k volnému tréninku od 16:15, zajišťuje Leoš Drozd (603388528)', 7, '0', '2011-10-12 22:00:00'),
(308, 1, 'Ze starých stránek', 'Byly posunuty časy lekcí s Miroslavem Hýžou v pátky o 15 min dříve.', 7, '0', '2011-10-12 22:00:00'),
(309, 1, 'Ze starých stránek', 'Lekce s Pavlou Landsfeldovou (tréninková nabídka) si můžete zapisovat do nedělního večera. Vzhledem k tomu, že v tento víkend (22.-23.) jsou soutěže v Uherském Hradišti a Drnovicích, zvažte možné tréninkové kolize a napište na mail hyzam@tkolymp.cz nebo na Facebook do infokanálu na který den chcete lekce vypsat. ', 7, '0', '2011-10-12 22:00:00'),
(310, 1, 'Ze starých stránek', '<b>Víkendové soustředění Loštice<br></b><br>  datum: 4.-6. 11. 2011<br> místo: Hotel Střelnice, Loštice<br> <br> Jak bylo domluveno na členské schůzi, během podzimu uspořádáme jedno víkendové soustředění v již známém areálu hotelu Střelnice v Lošticích. Soustředění bude od pátku (15 h) do neděle (cca 17 h) a bude pro VŠECHNY PÁRY našeho klubu. Jak pro začátečníky, tak pro vyspělé tanečníky, od dětí po dospěláky.  <br><br> Trénovat Vás budou naši kluboví trenéři (aby bylo soustředění cenově dostupné pro všechny). Cena bude stanovéna podle počtu přihlášených párů a bude se pohybovat okolo 1700 Kč/osobu.  <br><br> Strava a ubytování i tréninky budou probíhat stejně jako v létě v přímo v hotelu. Tréninky budou ušité na míru počtu a portfoliu přihlášených párů, lze očekávat dohromady cca 9 společných lekcí a 2-3 practisy. Rovněž bude po uzavření přihlášek možnost rezervace individuálních lekcí - zde na webu. <br><br> Přihlašujte se v sekci tábory a soustředění, <b>přihláška zde je ZÁVAZNÁ</b>, tzn., že pokud tam budete po termínu přihlášení zapsáni, budeme s Vámi počítat a v případě Vaší neúčasti po Vás budeme chtít objednané soustředění uhradit.  <br><br> <b>TERMÍN PŘIHLÁŠENÍ: neděle 23.10. 20:00', 7, '0', '2011-10-13 22:00:00'),
(311, 1, 'Ze starých stránek', '<b>Společné lekce ,,nad plán"</b><br><br>  Jak bylo avizováno, v sobotu 22.10. se v 16:00 na Slovanském gymnáziu uskuteční společná lekce standardních tanců s Pavlou Landsfeldovou, lekce bude trvat 90 min.<br><br>  Ve čtvrtek 27.10. proběhne společná lekce s Jardou Kučerou od 17:30 na baletním sále. Lekce bude trvat 60 minut. Individuální lekce si můžete zapisovat v sekci Tréninková nabídka. <br><br> Obě společné lekce jsou pro všechny členy žluté skupiny zdarma!', 7, '0', '2011-10-19 22:00:00'),
(312, 1, 'Ze starých stránek', 'Víkendové soustředění v Lošticích proběhne jen v případě zaplnění tréninkových skupin (alespoň 5 párů v dětech/juniorech, alespoň 5 párů žluté skupiny a alespoň 30 tanečníků celkem). Termín přihlášení je do neděle večer, přihlásit se můžete v sekci Tábory a soustředění.', 7, '0', '2011-10-19 22:00:00'),
(313, 1, 'Ze starých stránek', 'Ve čtvrtek 27.10. neproběhne tradiční practise a pilates z důvodu tréninků s Jardou Kučerou, v pátek neproběhnou žádné společné lekce z důvodu státního svátku. Pravidelné individuálky s Miroslavem Hýžou se ruší z důvodu taneční soutěže.', 7, '0', '2011-10-19 22:00:00'),
(314, 1, 'Ze starých stránek', 'Máme k dispozici termín tréninků s Filipem Karáskem a Sabinou Piškovou navíc. Jedná se o sobotu 29.10. Pokud o ně máte zájem, zarezervujte si je nejpozději do zítřejšího večera v sekci Tréninková nabídka.', 7, '0', '2011-10-23 22:00:00'),
(315, 1, 'Ze starých stránek', '<b>Soustředění v Lošticích je pro malý zájem (29 lidí) ZRUŠENO!</b><br><br>  Vzhledem k tomu, že většina dotázaných neúčastníků uvedla jako důvod neúčasti finance, pokusíme se soustředění uspořádat v Olomouci za menší peníz - odpadne strava a ubytování.  <br><br> 4.-6.11. na SGO a ZŠ Holečkova <br><br> Ceny soustředění:<br> Žlutá skupina: 8 společných lekcí, 2x individuálka, 2x practise, 800 Kč<br> Fialová (červená + modrá): 6 společných lekcí, 2x individuálka, 2x practise, 600 Kč<br> Nováčci: 8 společných lekcí, 1x individuálka: 500 Kč<br> Kroužek SGO: 8 společných lekcí, 400 Kč<br> <br> Přihlašujte se v sekci tábory a soustředění do neděle 30.10.', 7, '0', '2011-10-24 22:00:00'),
(316, 1, 'Ze starých stránek', 'Tréninky s Kamilou Limberkovou ve středu se pro nedostatečný zájem ruší, společnou lekci nahradí ve čtvrtek Jarda Kučera.', 7, '0', '2011-10-24 22:00:00'),
(317, 1, 'Ze starých stránek', 'Sobotní nabídka tréninků s Filipem Karáskem a Sabinou Piškovou nebyla dostatečně zaplněna a tréninky se tedy neuskuteční.', 7, '0', '2011-10-25 22:00:00'),
(318, 1, 'Ze starých stránek', '<b>Rozpis a informace k soustředění zde: </b><br><br> http://www.ulozto.cz/10842471/soustredeni-listopad-2011-pdf', 7, '0', '2011-10-30 23:00:00'),
(319, 1, 'Ze starých stránek', 'Upravená verze rozpisu soustředění: http://www.ulozto.cz/10874903/soustredeni-listopad-2011-pdf', 7, '0', '2011-11-02 23:00:00'),
(320, 1, 'Ze starých stránek', '<b>Důležité:<br></b> Na baletním sále je vyvěšen rozpis plateb ČSTS na rok 2012.<br> Dospělí: 		1400 Kč<br> Děti do 12 let:	        1050 Kč<br> Příspěvky budou vybírány během měsíce listopadu na společných lekcích!!! !!!Prosím dodržte termíny!!!', 7, '0', '2011-11-06 23:00:00'),
(321, 1, 'Ze starých stránek', 'Tento týden v úterý se z důvodu Martinovy účasti a kempu sportovního centra mládeže  neproběhnou individuální lekce STT. Společnou lekci ve stejném čase odučí Miroslav Hýža.', 7, '0', '2011-11-12 23:00:00'),
(322, 1, 'Ze starých stránek', 'Obě vedené hodiny červené skupiny (pondělí i středa) budou tento týden zaměřené na standardní tance.', 7, '0', '2011-11-12 23:00:00'),
(323, 1, 'Ze starých stránek', '<b>Příspěvky ČSTS - nové páry<br><br></b> Někteří členové již nebudou moci v novém roce startovat v hobby kategorii (max 2 roky) a je nutná jejich plnohodnotná registrace do Českého svazu tanečního sportu. Týká se to především těchto členů:<br>  Marek Chytil<br> Petra Slezáková<br> Vilém Šír<br> Nela Šírová<br> Richard Pánek<br><br>  Registraci doporučujeme i těmto letos neregistrovaným členům:<br> Mikuláš Molitor<br> Anna Pánková<br> Natálie Svatoňová<br> Michaela Pleváková<br> Jan Stibořík<br> Karel Flaška<br> Barbora Sedláková<br> Vít Spurný<br> Petra Navrátilová<br> Jakub Martiník<br> Michaela Franková<br>   <br> V případě Vaší registrace je nutné zaplatit společně s příspěvky i správní poplatek 50 Kč, vyplnit a odevzdat přihlášku a také žádost o soutěžní průkaz, které si buďto můžete stáhnout na: <br><br> http://www.csts.cz/cs/Informace/DokumentyKeStazeni <br><br> nebo vyžádat od Miroslava Hýži. <br> <br> Členové, kteří byli již dříve registrovaní v jiném klubu (Sedláková, Flaška, Pala, Brunclíková), musí vyřídit příspěvek přes svůj bývalý klub a následně zažádat o přestup či hostování.', 7, '0', '2011-11-12 23:00:00'),
(324, 1, 'Ze starých stránek', 'Příspěvky ČSTS - dovětek:<br><br> V případě neodevzdání příspěvků do konce listopadu, si budete muset vyřídit platbu a registraci sami, zatímco v opačném případě za Vás vše obstará klub. Pokud zaplatíte až v novém roce, bude Vám od svazu účtovaný evidenční poplatek 50 Kč, který v případě platby přes klub včas, neplatíte.', 7, '0', '2011-11-12 23:00:00'),
(325, 1, 'Ze starých stránek', 'Kdo ještě nedostal DVD s fotkami z Loštic, přihlaste se o ně tento týden na tréninku!', 7, '0', '2011-11-12 23:00:00'),
(326, 1, 'Ze starých stránek', 'Do nabídky tréninků byly vloženy lekce s Filipem a Sabkou na příští pátek. Deadline rezervací je neděle 20:00', 7, '0', '2011-11-13 23:00:00'),
(327, 1, 'Ze starých stránek', '<b>Příspěvky ČSTS - hobby páry:<br><br></b>  Všechny páry, které budou chtít v roce 2012 startovat v hobby soutěžích, žádáme odevzdat poplatek 200 Kč/osobu paní Jendrulkové na společných lekcích v listopadu. ', 7, '0', '2011-11-14 23:00:00'),
(328, 1, 'Ze starých stránek', 'Páteční tréninky s Luckou Benýškovou modré a červené (fialové) skupiny, budou sloučeny tentokrát do jedné hodiny - od 16:30 na ZŠ Holečkova.', 7, '0', '2011-11-14 23:00:00'),
(329, 1, 'Ze starých stránek', 'Úterní tréninky s Martinem Odstrčilem odpadají z důvodu nemoci, společnou hodinu odučí Miroslav Hýža. Pokud Martinův zdravotní stav dovolí, přijede v sobotu. Počítat můžete jak se společnou lekcí (která je v plánu na 10:30), tak s individuálkami. Pokud má někdo zájem o lekce nad (úterní) plán, kontaktujte Miroslava Hýžu (hyzam@tkolymp.cz nebo facebook)', 7, '0', '2011-11-21 23:00:00'),
(330, 1, 'Ze starých stránek', 'Tréninky s Filipem a Sabinou v pondělí nebudou. Náhradní termín je příští pátek. Individuální lekce budou rozděleny mezi tento a příští pátek. Společná hodina LAT proběhne v pátek 7.12. od 17:30 na Slovanském gymnáziu', 7, '0', '2011-11-23 23:00:00'),
(331, 1, 'Ze starých stránek', 'Společná hodina STT pro žlutou skupinu bude v sobotu od 14:00 na SGO.', 7, '0', '2011-11-23 23:00:00'),
(332, 1, 'Ze starých stránek', 'Upozorňujeme, že se blíží poslední termín odevzdávání příspěvků Českého svazu tanečního sportu (více viz níže).', 7, '0', '2011-11-23 23:00:00'),
(333, 1, 'Ze starých stránek', 'Dnes není pilates (Petra má zdravotní problémy), od 16:15 proběhne volný trénink, následně practise jako obvykle.', 7, '0', '2011-11-30 23:00:00'),
(334, 1, 'Ze starých stránek', 'Tréninky s Kamilou pro dnešek odpadají, s největší pravděpodobností budou nahrazeny příští středu.', 7, '0', '2011-12-06 23:00:00');

-- --------------------------------------------------------

--
-- Table structure for table `users`
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
  `u_ban` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_lock` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  `u_confirmed` enum('0','1') COLLATE cp1250_czech_cs NOT NULL DEFAULT '0',
  PRIMARY KEY (`u_id`),
  UNIQUE KEY `u_login` (`u_login`),
  KEY `u_jmeno` (`u_jmeno`),
  KEY `u_prijmeni` (`u_prijmeni`)
) ENGINE=MyISAM  DEFAULT CHARSET=cp1250 COLLATE=cp1250_czech_cs AUTO_INCREMENT=2 ;

--
-- Dumping data for table `users`
--

INSERT INTO `users` (`u_id`, `u_login`, `u_pass`, `u_jmeno`, `u_prijmeni`, `u_pohlavi`, `u_email`, `u_telefon`, `u_poznamky`, `u_aktu`, `u_level`, `u_ban`, `u_lock`, `u_confirmed`) VALUES
(1, 'superadmin', '9947a7bc1549a54e7299fe9a3975c8655430ade0', 'Hlavní', 'Administrátor', 'm', 'tkolymp@tkolymp.cz', '', 'nnn', '2011-12-08 22:18:19', 99, '0', '0', '1');
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
