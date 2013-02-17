<?php
class Controller_Admin_Ankety implements Controller_Interface {
	function __construct() {
		Permissions::checkError('ankety', P_OWNED);
	}
    function view($id = null) {
    	if(empty($_POST)) {
			include("files/Admin/Ankety/Display.inc");
			return;
		}
		
		switch(post("action")) {
			case 'edit':
				$ankety = post('ankety');
				if($ankety[0])
					View::redirect('/admin/ankety/edit/' . $ankety[0]);
				break;
		
			case 'remove':
				if(!is_array(post('ankety')))
					break;
				$url = '/admin/ankety/remove?';
				foreach(post('ankety') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		include("files/Admin/Ankety/Display.inc");
    }
    function add($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Ankety/Form.inc');
			return;
		}
		$visible = (bool) post("visible");
		if(!Permissions::check('ankety', P_ADMIN)) {
			$visible = false;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
		}
		
		DBAnkety::addAnketa(User::getUserID(), post('jmeno'), post('text'), '0',
			(bool) $visible);
		$data = DBAnkety::getLatestAnketa();
		
		if($visible) {
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal anketu ' .
				post('jmeno'));
		}
		if(post("add_text")) {
			DBAnkety::addAnketaItem($data['ak_id'], post("add_text"));
			unset($_POST["add_text"]);
		}
		View::redirect('/admin/ankety/edit/' . $data['ak_id'], 'Anketa přidána');
    }
    function edit($id = null) {
		if(!$id || !($data = DBAnkety::getSingleAnketa($id)))
			View::redirect('/admin/ankety', 'Anketa s takovým ID neexistuje');
		
		Permissions::checkError('ankety', P_OWNED, $data['ak_kdo']);
		
		$items = DBAnkety::getAnketaItems($id);
			
		if(empty($_POST)) {
			post("jmeno", $data["ak_jmeno"]);
			post("text", $data["ak_text"]);
			post("visible", $data["ak_visible"]);
			
			include("files/Admin/Ankety/Form.inc");
			return;
		}
		
		if(post("remove") > 0) {
			DBAnkety::removeAnketaItem(post("remove"));
			$items = DBAnkety::getAnketaItems($id);
			$changed = true;
		}
		
		foreach($items as $item) {
			if(post($item["aki_id"] . "-text") != $item["aki_text"]) {
				DBAnkety::editAnketaItem($item["aki_id"], post($item["aki_id"] . "-text"));
				$changed = true;
			}
		}
		$items = DBAnkety::getAnketaItems($id);
		
		if(post("add_text")) {
			DBAnkety::addAnketaItem($id, post("add_text"));
			post('add_text', null);
			$items = DBAnkety::getAnketaItems($id);
			$changed = true;
		}
		
		$visible = (bool) post('visible');
		$visible_prev = $data['ak_visible'];
		if(!Permissions::check('ankety', P_ADMIN) && $visible != $visible_prev) {
			$visible = $visible_prev;
			View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
		}
		
		if($visible != $visible_prev || post('jmeno') != $data['ak_jmeno'] ||
				post('text') != $data['ak_text']) {
			DBAnkety::editAnketa($id, post('jmeno'), post('text'), '0', $visible);
			$data = DBAnkety::getSingleAnketa($id);
			$changed = true;
		}
		
		if(isset($changed) && $changed) {
			if($visible) {
				if(!$visible_prev)
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal anketu ' .
						post('jmeno'));
				else
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil anketu ' .
						$data['ak_jmeno']);
			} elseif(!$visible && $visible_prev) {
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil anketu ' .
						$data['ak_jmeno']);
			}
		}
		
		post("jmeno", $data["ak_jmeno"]);
		post("text", $data["ak_text"]);
		post("visible", $data["ak_visible"]);
		
		include("files/Admin/Ankety/Form.inc");
    }
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Ankety/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('ankety')))
			View::redirect('/admin/ankety');
		foreach(post('ankety') as $id) {
			$data = DBAnkety::getSingleAnketa($id);
			
			if(Permissions::check('ankety', P_OWNED, $data['ak_kdo'])) {
				DBAnkety::removeAnketa($item);
				if($data['ak_visible'])
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil anketu ' .
						$data['ak_jmeno']);
			} else {
				$error = true;
			}
		}
		if(isset($error) && $error)
			View::viewError(ER_AUTHORIZATION);
		
		View::redirect('/admin/ankety', 'Ankety odebrány');
	}
}
?>