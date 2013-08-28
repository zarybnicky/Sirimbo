<?php
class Controller_Admin extends Controller_Abstract {
	function __construct() {
		Permissions::checkError('nastenka', P_OWNED);
	}
	function view($id = null) {
		$this->render('files/View/Admin/Home.inc');
	}
	function sidebar() {
		$s = new Sidebar();
		
		echo $s->menuHeader();
		echo $s->menuItem('Správa uživatelů',	'/admin/users',		'users', P_OWNED);
		echo $s->menuItem('Správa skupin',		'/admin/skupiny',	'skupiny', P_OWNED);
		echo $s->menuItem('Správa plateb',		'/admin/platby',	'platby', P_OWNED);
		echo $s->menuItem('Správa párů',		'/admin/pary',		'pary', P_OWNED);
		echo $s->menuItem('Správa článků',		'/admin/aktuality',	'aktuality', P_OWNED);
		echo $s->menuItem('Správa nástěnky',	'/admin/nastenka',	'nastenka', P_OWNED);
		echo $s->menuItem('Správa rozpisů',		'/admin/rozpis',	'rozpis', P_OWNED);
		echo $s->menuItem('Správa nabídky',		'/admin/nabidka',	'nabidka', P_OWNED);
		echo $s->menuItem('Správa akcí',		'/admin/akce',		'akce', P_OWNED);
		echo $s->menuItem('Správa galerie',		'/admin/galerie',	'galerie', P_OWNED);
		echo $s->menuItem('Správa dokumentů',	'/admin/dokumenty',	'dokumenty', P_OWNED);
		echo $s->menuItem('Správa anket',		'/admin/ankety',	'ankety', P_OWNED);
		echo $s->menuItem('Správa oprávnění',	'/admin/permissions','permissions', P_OWNED);
		echo $s->menuItem('Konzole',			'/admin/konzole',	'konzole', P_OWNED);
		
		echo $s->commonItems();
	}
}
?>