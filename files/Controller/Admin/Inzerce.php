<?php
class Controller_Admin_Inzerce implements Controller_Interface {
	function __construct() {
		Permissions::checkError('inzerce', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include("files/Admin/Inzerce/Display.inc");
			return;
		}
		
		switch(post("action")) {
			case 'edit':
				$inzerce = post('inzerce');
				if($inzerce[0])
					View::redirect('/admin/inzerce/edit/' . $inzerce[0]);
				break;
			case 'remove':
				if(!is_array(post('inzerce')))
					break;
				$url = '/admin/inzerce/remove?';
				foreach(post('inzerce') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		include("files/Admin/Inzerce/Display.inc");
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$user_data = DBUser::getUserData(post('reg'));
		$jmeno = post('jmeno') ? post('jmeno') : $user_data['u_jmeno'];
		$prijmeni = post('prijmeni') ? post('prijmeni') : $user_data['u_prijmeni'];
		
		DBInzerce::addInzerat(post('kat'), post('reg'), $jmeno, $prijmeni,
			post('nadpis'), post('text'), serialize(array()), $od, $do,
			'', (bool) post('visible'), '1');
		View::redirect('/admin/inzerce', 'Inzerát přidán');
		
	}
	function edit($id = null) {
		if(!$id || !($data = DBInzerce::getSingleInzerat($id)))
			View::redirect('/admin/inzerce', 'Inzerát s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('kat', $data['i_kat']);
			post('reg', $data['i_reg']);
			post('jmeno', $data['i_jmeno']);
			post('prijmeni', $data['i_prijmeni']);
			post('nadpis', $data['i_nadpis']);
			post('text', $data['i_text']);
			post('od', $data['i_od']);
			post('do', $data['i_do']);
			post('pass', $data['i_pass']);
			post('visible', $data['i_visible']);
			post('confirmed', $data['i_confirmed']);
			
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Inzerce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$user_data = DBUser::getUserData(post('reg'));
		$jmeno = $user_data['u_jmeno'];
		$prijmeni = $user_data['u_prijmeni'];
		
		DBInzerce::editInzerat($id, post('kat'), post('reg'), $jmeno, $prijmeni,
			post('nadpis'), post('text'), serialize(array()), $od, $do,
			$data['i_pass'], (bool) post('visible'), '1');
		View::redirect("/admin/inzerce", "Inzerát upraven");
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Inzerce/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('inzerce')))
			View::redirect('/admin/inzerce');
		foreach(post('inzerce') as $id)
			DBInzerce::removeInzerat($id);
		View::redirect('/admin/inzerce', 'Inzeráty odebrány');
	}
	function unconfirmed($id = null) {
		if(empty($_POST) || !in_array(post('action'), array('confirm', 'remove'))) {
			include('files/Admin/Inzerce/DisplayNew.inc');
			return;
		}
		if(!is_array(post('inzerce')))
			View::redirect('/admin/inzerce/unconfirmed', 'Nevybrali jste žádné inzeráty');
		
		if(post('action') == 'confirm') {
			foreach(post('inzerce') as $id) {
				$visible = post($id . '-visible');
				DBInzerce::confirmInzerat($id, (bool) $visible);
			}
			View::redirect('/admin/inzerce', 'Inzeráty potvrzeny');
		} elseif(post('action') == 'remove') {
			$url = '/admin/inzerce/remove?';
			foreach(post('inzerce') as $id)
				$url .= '&u[]=' . $id;
			View::redirect($url);
		}
	}
	
	private function checkData($data, $action = 'add') {
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		
		$f = new Form();
		$f->checkLength(post('nadpis'), 1, 255, 'Špatná délka nadpisu', 'nadpis');
		$f->checkInArray(post('kat'), array(1,2,3,4), 'Neplatná kategorie', 'kat');
		$f->checkNumeric(post('reg'), 'Neplatný uživatel', 'reg');
		$f->checkDate($od, 'Neplatný formát data ("Od")', 'od');
		if($do) $f->checkDate($do, 'Neplatný formát data ("Do")', 'do');
		
		return $f->isValid() ? true : $f;
	}
}
?>