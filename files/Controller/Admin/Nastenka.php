<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Nastenka extends Controller_Admin {
	function __construct() {
		Permissions::checkError('nastenka', P_OWNED);
	}
	function view($id = null) {	
		switch(post('action')) {
			case 'remove':
				if(!is_array(post('nastenka')))
					break;
				foreach(post('nastenka') as $item) {
					$data = DBNastenka::getSingleNastenka($item);
					if(!Permissions::check('nastenka', P_OWNED, $data['up_kdo'])) {
						$error = true;
						continue;
					}
					
					DBNastenka::removeNastenka($item);
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() .
						' smazal příspěvek z nástěnky');
				}
				if(isset($error) && $error)
					throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
				$this->redirect()->setRedirectMessage('Příspěvky odebrány');
				break;
			case 'edit':
				$nastenka = post('nastenka');
				if($nastenka[0])
					$this->redirect('/admin/nastenka/edit/' . $nastenka[0]);
				break;
		}
		$pager = new Paging(new PagingAdapterDBSelect('DBNastenka'));
		$pager->setCurrentPageField('p');
		$pager->setItemsPerPageField('c');
		$pager->setDefaultItemsPerPage(20);
		$pager->setPageRange(5);
		$data = $pager->getItems();
		foreach($data as &$row) {
			$new_data = array(
					'canEdit' => Permissions::check('nastenka', P_OWNED, $row['up_kdo']),
					'header' => $row['up_nadpis'],
					'fullName' => $row['u_jmeno'] . ' ' . $row['u_prijmeni'],
					'timestampAdd' => formatTimestamp($row['up_timestamp_add'], true),
					'timestampEdit' => formatTimestamp($row['up_timestamp'], true)
			);
			if($new_data['canEdit'])
				$new_data['checkBox'] = '<input type="checkbox" name="nastenka[]" value="' . $row['up_id'] . '" />';
			else
				$new_data['checkBox'] = '&nbsp;&#10799;';
			
			$skupiny = DBNastenka::getNastenkaSkupiny($row['up_id']);
			$new_data['groups'] = '';
			foreach($skupiny as $skupina)
				$new_data['groups'] .= getColorBox($skupina['ups_color'], $skupina['ups_popis']);
			
			$row = $new_data;
		}
		$this->render('files/View/Admin/Nastenka/Overview.inc', array(
				'showMenu' => !TISK,
				'data' => $data,
				'navigation' => $pager->getNavigation()
		));
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			if(!empty($_POST))
				$this->redirect()->setRedirectMessage($f->getMessages());
			$this->render('files/View/Admin/Nastenka/Form.inc', array(
					'action' => Request::getAction(),
					'returnURL' => Request::getReferer(),
					'skupiny' => DBSkupiny::getSkupiny()				
			));
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
		
		$this->redirect(getReturnURI('/admin/nastenka'), 'Příspěvek úspěšně přidán');
	}
	function edit($id = null) {
		if(!$id || !($data = DBNastenka::getSingleNastenka($id)))
			$this->redirect(getReturnURI('/admin/nastenka'), 'Nástěnka s takovým ID neexistuje');
		
		Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
		$skupiny = DBNastenka::getNastenkaSkupiny($id);
		
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'edit'))) {
			if(empty($_POST)) {
				post('id', $id);
				post('nadpis', $data['up_nadpis']);
				post('text', stripslashes($data['up_text']));
				foreach($skupiny as $skupina) {
					post('sk-' . $skupina['ups_id_skupina'], 1);
				}
				post('lock', $data['up_lock']);
			} else {
				$this->redirect()->setRedirectMessage($f->getMessages());
			}
			$this->render('files/View/Admin/Nastenka/Form.inc', array(
					'action' => Request::getAction(),
					'returnURL' => Request::getReferer(),
					'skupiny' => DBSkupiny::getSkupiny()				
			));
			return;
		}
		$skupiny_old = $skupiny;
		$skupiny = array();
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
		
		$this->redirect(getReturnURI('/admin/nastenka'), 'Příspěvek úspěšně upraven');
	}
	
	private function checkData($data, $action = 'add') {
		$f = new Form();
		$f->checkNotEmpty(post('nadpis'), 'Zadejte nadpis', 'nadpis');
		$f->checkNotEmpty(post('text'), 'Zadejte nějaký text', 'text');
		return $f->isValid() ? true : $f;
	}
}
?>