<?php
$sitemap_static = array();
$sitemap_dynamic = array(
	"home"				=> "files/Main/Home/Main.inc"
);
/*$sitemap_static = array(
	"forum"				=> "files/Main/Forum/Main.inc",
	"done"				=> "files/Main/RegistraceDone.inc"
);
$sitemap_dynamic = array(
	"home"				=> "files/Main/Home/Main.inc",
	"home/ankety"		=> "files/Main/Ankety.inc",
	"home/zajemci"		=> "files/Main/Home/Zajemci.inc",
	"home/odkazy"		=> "files/Main/Home/Odkazy.inc",

	"oklubu"				=> "files/Main/OKlubu/Main.inc",
	"oklubu/historie"		=> "files/Main/OKlubu/Historie.inc",
	"oklubu/mistrovstvi"	=> "files/Main/OKlubu/Mistrovstvi.inc",
	"oklubu/liga"			=> "files/Main/OKlubu/Liga.inc",
	"oklubu/druzstva"		=> "files/Main/OKlubu/Druzstva.inc",
	"oklubu/uspechy"		=> "files/Main/OKlubu/VCislech.inc",
	"oklubu/treneri/klubovi"=> "files/Main/OKlubu/TreneriInt.inc",
	"oklubu/treneri/externi"=> "files/Main/OKlubu/TreneriExt.inc",
	"oklubu/saly"			=> "files/Main/OKlubu/Saly.inc",
	"oklubu/rada"			=> "files/Main/OKlubu/Main.inc",

	"aktuality"			=> "files/Main/Aktualne/Main.inc",
	"aktuality/posledni"=> "files/Main/Aktualne/Main.inc",
	"aktuality/videa"	=> "files/Main/Aktualne/Main.inc",
	"aktuality/zpravy"	=> "files/Main/Aktualne/Main.inc",

	"fotogalerie"		=> "files/Main/Fotogalerie/Main.inc",
	"fotogalerie/foto"	=> "files/Main/Fotogalerie/Main.inc",

	"kontakt"			=> "files/Main/Kontakt/Main.inc",

	"inzerce"			=> "files/Main/Inzerce/Main.inc",
	"inzerce/posledni"	=> "files/Main/Inzerce/Main.inc",
	"inzerce/prodam"	=> "files/Main/Inzerce/Main.inc",
	"inzerce/koupim"	=> "files/Main/Inzerce/Main.inc",
	"inzerce/partner"	=> "files/Main/Inzerce/Main.inc",
	"inzerce/partnerka"	=> "files/Main/Inzerce/Main.inc",
	"inzerce/add"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/edit"		=> "files/Main/Inzerce/Main.inc",
	"inzerce/remove"	=> "files/Main/Inzerce/Main.inc",

	"nabizime"				=> "files/Main/Nabizime/Main.inc",
	"nabizime/vystoupeni"	=> "files/Main/Nabizime/Vystoupeni.inc",
	"nabizime/individualky"	=> "files/Main/Nabizime/Individualky.inc",
	"nabizime/seminare"		=> "files/Main/Nabizime/Seminare.inc",
	"nabizime/soustredeni"	=> "files/Main/Nabizime/Seminare.inc",

	"error"				=> "files/Main/Error.inc",
	"logout"			=> "files/Main/Logout.inc",
	"registrace"		=> "files/Main/Registrace.inc",
	"member/download"	=> "files/Member/Download.inc",
	
	"member/home"		=> "files/Member/Home.inc",
	"member/nastenka"	=> "files/Member/Nastenka.inc",
	"member/rozpis"		=> "files/Member/Rozpis.inc",
	"member/nabidka"	=> "files/Member/Nabidka.inc",
	"member/akce"		=> "files/Member/Akce.inc",
	"member/dokumenty"	=> "files/Member/Dokumenty.inc",
	"member/zebricek"	=> "files/Member/Zebricek.inc",
	"member/clenove"	=> "files/Member/Clenove.inc",
	"member/pary"		=> "files/Member/Pary.inc",

	"member/profil"			=> "files/Member/Profil.inc",
	"member/profil/edit"	=> "files/Member/Profil.inc",
	"member/profil/heslo"	=> "files/Member/Profil.inc",
	"member/profil/par"		=> "files/Member/Profil.inc",
	"member/profil/par/partner"	=> "files/Member/Profil.inc",
	"member/profil/par/body"	=> "files/Member/Profil.inc",
	"member/profil/par/zadost"	=> "files/Member/Profil.inc",
	"member/profil/inzerce"		=> "files/Member/Profil.inc",
	"member/profil/inzerce/add"	=> "files/Member/Profil.inc",
	"member/profil/inzerce/edit"	=> "files/Member/Profil.inc",
	"member/profil/inzerce/remove"	=> "files/Member/Profil.inc",

	"admin/home"				=> "files/Admin/Main.inc",

	"admin/dokumenty"			=> "files/Admin/Dokumenty/Main.inc",
	"admin/dokumenty/edit"		=> "files/Admin/Dokumenty/Main.inc",

	"admin/nastenka"			=> "files/Admin/Nastenka/Main.inc",
	"admin/nastenka/add"		=> "files/Admin/Nastenka/Main.inc",
	"admin/nastenka/edit"		=> "files/Admin/Nastenka/Main.inc",

	"admin/nabidka"				=> "files/Admin/Nabidka/Main.inc",
	"admin/nabidka/add"			=> "files/Admin/Nabidka/Main.inc",
	"admin/nabidka/edit"		=> "files/Admin/Nabidka/Main.inc",
	"admin/nabidka/detail"		=> "files/Admin/NabidkaDetail/Main.inc",

	"admin/novinky/remove"		=> "files/Admin/Novinky/Main.inc",

	"admin/pary"				=> "files/Admin/Pary/Main.inc",
	"admin/pary/edit"			=> "files/Admin/Pary/Main.inc",

	"admin/rozpis"				=> "files/Admin/Rozpis/Main.inc",
	"admin/rozpis/add"			=> "files/Admin/Rozpis/Main.inc",
	"admin/rozpis/edit"			=> "files/Admin/Rozpis/Main.inc",
	"admin/rozpis/detail"		=> "files/Admin/RozpisDetail/Main.inc",

	"admin/akce"				=> "files/Admin/Akce/Main.inc",
	"admin/akce/add"			=> "files/Admin/Akce/Main.inc",
	"admin/akce/edit"			=> "files/Admin/Akce/Main.inc",
	"admin/akce/detail"			=> "files/Admin/AkceDetail/Main.inc",
	"admin/akce/dokumenty"		=> "files/Admin/AkceDokumenty/Main.inc",

	"admin/users"				=> "files/Admin/Users/Main.inc",
	"admin/users/add"			=> "files/Admin/Users/Main.inc",
	"admin/users/edit"			=> "files/Admin/Users/Main.inc",
	"admin/users/new"			=> "files/Admin/Users/Main.inc",
	"admin/users/temporary"		=> "files/Admin/Users/Temporary.inc",

	"admin/ankety"				=> "files/Admin/Ankety/Main.inc",
	"admin/ankety/add"			=> "files/Admin/Ankety/Main.inc",
	"admin/ankety/edit"			=> "files/Admin/Ankety/Main.inc",

	"admin/inzerce"				=> "files/Admin/Inzerce/Main.inc",
	"admin/inzerce/add"			=> "files/Admin/Inzerce/Main.inc",
	"admin/inzerce/edit"		=> "files/Admin/Inzerce/Main.inc",
	"admin/inzerce/new"			=> "files/Admin/Inzerce/Main.inc",

	"admin/aktuality"			=> "files/Admin/Aktuality/Main.inc",
	"admin/aktuality/add"		=> "files/Admin/Aktuality/Main.inc",
	"admin/aktuality/edit"		=> "files/Admin/Aktuality/Main.inc",

	"admin/galerie"				=> "files/Admin/Galerie/Main.inc",
	"admin/galerie/adddir"		=> "files/Admin/Galerie/Main.inc",
	"admin/galerie/edit"		=> "files/Admin/Galerie/Main.inc",
	"admin/galerie/upload"		=> "files/Admin/Galerie/Main.inc",
	"admin/galerie/scan"		=> "files/Admin/Galerie/Main.inc",
	"admin/galerie/process"		=> "files/Admin/Galerie/Main.inc"
);*/
?>