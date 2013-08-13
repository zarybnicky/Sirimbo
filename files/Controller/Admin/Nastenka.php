<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Nastenka extends Controller_Admin {
	function __construct() {
		Permissions::checkError('nastenka', P_OWNED);
	}
	function view($id = null) {		
		if(empty($_POST)) {
			include('files/Admin/Nastenka/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'remove':
				if(!is_array(post('nastenka')))
					break;
				
				foreach(post('nastenka') as $item) {
					$data = DBNastenka::getSingleNastenka($item);
					if(!Permissions::check('nastenka', P_OWNED, $data['up_kdo']))
						$error = true;
						
					DBNastenka::removeNastenka($item);
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() .
						' smazal příspěvek z nástěnky');
				}
				if(isset($error) && $error)
					throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
				notice('Příspěvky odebrány');
				break;
			
			case 'edit':
				$nastenka = post('nastenka');
				if($nastenka[0])
					View::redirect('/admin/nastenka/edit/' . $nastenka[0]);
				break;
		}
		
		include('files/Admin/Nastenka/Display.inc');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Nastenka/Form.inc');
			return;
		}
		
		$id = DBNastenka::addNastenka(User::getUserID(), post('nadpis'),
			post('text'), post('lock') ? 1 : 0);
		
		$skupiny = DBSkupiny::getSkupiny();
		foreach($skupiny as $skupina) {
			if(!post('sk-' . $skupina['us_id']))
				continue;
			DBNastenka::addNastenkaSkupina($id, $skupina['us_id'],
				$skupina['us_color'], $skupina['us_popis']);
		}
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() .
			' přidal příspěvek na nástěnku');
		
		View::redirect(getReturnURI('/admin/nastenka'),
			'Příspěvek úspěšně přidán');
		
	}
	function edit($id = null) {
		if(!$id || !($data = DBNastenka::getSingleNastenka($id)))
			View::redirect(getReturnURI('/admin/nastenka'), 'Nástěnka s takovým ID neexistuje');
		
		Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
		
		if(empty($_POST)) {
			$skupiny = DBNastenka::getNastenkaSkupiny($id);
						
			post('id', $id);
			post('nadpis', $data['up_nadpis']);
			post('text', stripslashes($data['up_text']));
			foreach($skupiny as $skupina) {
				post('sk-' . $skupina['ups_id_skupina'], 1);
			}
			post('lock', $data['up_lock']);
			include('files/Admin/Nastenka/Form.inc');
			return;
		}
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Nastenka/Form.inc');
			return;
		}
		$skupiny = array();
		$skupiny_old = DBNastenka::getNastenkaSkupiny($id);
		$skupiny_vse = DBSkupiny::getSkupiny();
		
		foreach($skupiny_old as $skupina) {
			$skupiny[$skupina['ups_id_skupina']] = $skupina['ups_id'];
		}
		$skupiny_old = $skupiny;
		unset($skupiny);
		
		foreach($skupiny_vse as $skupina) {
			if(post('sk-' . $skupina['us_id']) && isset($skupiny_old[$skupina['us_id']])) {
				continue;
			} elseif(post('sk-' . $skupina['us_id']) && !isset($skupiny_old[$skupina['us_id']])) {
				DBNastenka::addNastenkaSkupina($id, $skupina['us_id'],
					$skupina['us_color'], $skupina['us_popis']);
			} elseif(!post('sk-' . $skupina['us_id']) && isset($skupiny_old[$skupina['us_id']])) {
				DBNastenka::removeNastenkaSkupina($skupiny_old[$skupina['us_id']]);
			}
		}
		
		DBNastenka::editNastenka($id, post('nadpis'), post('text'), (post('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil příspěvek na nástěnce');
		
		View::redirect(getReturnURI('/admin/nastenka'), 'Příspěvek úspěšně upraven');
	}
	
	private function checkData($data, $action = 'add') {
		$f = new Form();
		$f->checkNotEmpty(post('nadpis'), 'Zadejte nadpis', 'nadpis');
		$f->checkNotEmpty(post('text'), 'Zadejte nějaký text', 'text');
		return $f->isValid() ? true : $f;
	}
}
?>