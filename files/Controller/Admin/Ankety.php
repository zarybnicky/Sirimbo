<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Ankety extends Controller_Admin {
	function __construct() {
		Permissions::checkError('ankety', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			$this->render("files/Admin/Ankety/Display.inc");
			return;
		}
		
		switch(post("action")) {
			case 'save':
				$items = DBAnkety::getAnkety();
				
				foreach($items as $item) {
					$id = $item['ak_id'];
					if((bool) post($id) != $item['ak_visible'])
						DBAnkety::editAnketa($id, $item['ak_jmeno'], $item['ak_text'], $item['ak_kdo'], post($id) ? '1' : '0');
				}
				break;
			case 'edit':
				$ankety = post('ankety');
				if($ankety[0])
					$this->redirect('/admin/ankety/edit/' . $ankety[0]);
				break;
		
			case 'remove':
				if(!is_array(post('ankety')))
					break;
				$url = '/admin/ankety/remove?';
				foreach(post('ankety') as $id)
					$url .= '&u[]=' . $id;
				$this->redirect($url);
				break;
		}
		$this->render("files/Admin/Ankety/Display.inc");
	}
	function add($id = null) {
		if(empty($_POST)) {
			$this->render('files/Admin/Ankety/Form.inc');
			return;
		}
		$visible = (bool) post("visible");
		if(!Permissions::check('ankety', P_ADMIN)) {
			$visible = false;
			$this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
		}
		
		DBAnkety::addAnketa(User::getUserID(), post('jmeno'), post('text'), '0',
			(bool) $visible);
		$data = DBAnkety::getLatestAnketa();
		
		if($visible) {
			$n = new Novinky(User::getUserID());
			$n->ankety()->add(post('jmeno'));
		}
		if(post("add_text")) {
			DBAnkety::addAnketaItem($data['ak_id'], post("add_text"));
			unset($_POST["add_text"]);
		}
		$this->redirect('/admin/ankety/edit/' . $data['ak_id'], 'Anketa přidána');
	}
	function edit($id = null) {
		if(!$id || !($data = DBAnkety::getSingleAnketa($id)))
			$this->redirect('/admin/ankety', 'Anketa s takovým ID neexistuje');
		
		Permissions::checkError('ankety', P_OWNED, $data['ak_kdo']);
		
		$items = DBAnkety::getAnketaItems($id);
			
		if(empty($_POST)) {
			post("jmeno", $data["ak_jmeno"]);
			post("text", $data["ak_text"]);
			post("visible", $data["ak_visible"]);
			
			$this->render("files/Admin/Ankety/Form.inc");
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
			$this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
		}
		
		if($visible != $visible_prev || post('jmeno') != $data['ak_jmeno'] ||
				post('text') != $data['ak_text']) {
			DBAnkety::editAnketa($id, post('jmeno'), post('text'), '0', $visible);
			$data = DBAnkety::getSingleAnketa($id);
			$changed = true;
		}

		$n = new Novinky(User::getUserID());
		if(isset($changed) && $changed) {
			if($visible) {
				if(!$visible_prev)
					$n->ankety()->add($data['ak_jmeno']);
				else
					$n->ankety()->edit($data['ak_jmeno']);
			} elseif(!$visible && $visible_prev) {
					$n->ankety()->remove($data['ak_jmeno']);
			}
		}
		post("jmeno", $data["ak_jmeno"]);
		post("text", $data["ak_text"]);
		post("visible", $data["ak_visible"]);
		
		$this->render("files/Admin/Ankety/Form.inc");
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			$this->render('files/Admin/Ankety/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('ankety')))
			$this->redirect('/admin/ankety');
		foreach(post('ankety') as $id) {
			$data = DBAnkety::getSingleAnketa($id);
			
			if(Permissions::check('ankety', P_OWNED, $data['ak_kdo'])) {
				DBAnkety::removeAnketa($item);
				if($data['ak_visible']) {
					$n = new Novinky(User::getUserID());
					$n->ankety()->remove($data['ak_jmeno']);
				}
			} else {
				$error = true;
			}
		}
		if(isset($error) && $error)
			throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
		
		$this->redirect('/admin/ankety', 'Ankety odebrány');
	}
}
?>