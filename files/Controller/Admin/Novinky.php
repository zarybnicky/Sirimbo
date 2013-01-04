<?php
class Controller_Admin_Novinky implements Controller_Interface {
	function view($id = null) {
		if(!is_numeric(get('id')))
			View::redirect("/member/home", "Novinka s daným ID neexistuje");
		DBNovinky::removeNovinka(get('id'));
		View::redirect("/member/home", "Novinka odstraněna");
	}
}
?>