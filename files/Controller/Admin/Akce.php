<?php
class Controller_Admin_Akce implements Controller_Interface {
	function __construct() {
		Permissions::checkError('akce', P_OWNED);
	}
    function view($id = null) {
        if(empty($_POST)) {
        	include('files/Admin/Akce/Display.inc');
        	return;
        }
        switch(post('action')) {
			case 'remove':
				if(!is_array(post('akce')))
					break;
				$url = '/admin/akce/remove?';
				foreach(post('akce') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
        	
        	case "edit":
        		$akce = post('akce');
        		if($akce[0])
        			View::redirect('/admin/akce/edit/' . $akce[0]);
        		break;
        		
        	case "edit_detail":
        		$akce = post("akce");
        		if($akce[0])
        			View::redirect('/admin/akce/detail/' . $akce[0]);
        		break;
        		
        	case "edit_doku":
        		$akce = post("akce");
        		if($akce[0])
        			View::redirect('/admin/akce/dokumenty/' . $akce[0]);
                break;
        }
    }
    function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Akce/Form.inc');
			return;
		}
        
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
        
		DBAkce::addAkce(post('jmeno'), post('kde'), post('info'),
			$od, $do, post('kapacita'), post('dokumenty'),
			(post('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal klubovou akci "' .
			post('jmeno') . '"');
		View::redirect('/admin/akce', 'Akce přidána');
    }
    function edit($id = null) {
		if(!$id || !($data = DBAkce::getSingleAkce($id)))
			View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('id', $id);
			post('jmeno', $data['a_jmeno']);
			post('kde', $data['a_kde']);
			post('info', $data['a_info']);
			post('od', $data['a_od']);
			post('do', $data['a_do']);
			post('kapacita', $data['a_kapacita']);
			post('dokumenty', unserialize($data['a_dokumenty']));
			post('lock', $data['a_lock']);
			
			include('files/Admin/Akce/Form.inc');
			return;
		}
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Akce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
        
		DBAkce::editAkce($id, post('jmeno'), post('kde'), post('info'),
			$od, $do, post('kapacita'), post('dokumenty'),
			(post('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' změnil detaily akce "' .
			post('jmeno') . '"');
		View::redirect('/admin/akce', 'Akce upravena');
    }
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Akce/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('akce')))
			View::redirect('/admin/akce');
		foreach(post('akce') as $id) {
			if(Permissions::check('akce', P_OWNED)) {
				$data = DBAkce::getSingleAkce($id);
				DBAkce::removeAkce($id);
				
				if(strcmp($data['a_od'], date('Y-m-d')) > 0)
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() .
						' zrušil klubovou akci "' . $data['a_jmeno'] . '"');
			} else {
				$error = true;
			}
		}
		if(isset($error) && $error)
			View::viewError(ER_AUTHORIZATION);
		
		View::redirect('/admin/akce', 'Akce odebrány');
    }
    
    private function checkData($data, $action = 'add') {
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
        
		$f = new Form();
		$f->checkLength(post('jmeno'), 1, 255, 'Špatná délka jména akce', 'jmeno');
		$f->checkLength(post('kde'), 1, 255, 'Špatná délka místa konání', 'kde');
		$f->checkDate($od, 'Špatný formát data ("Od")', 'od');
		if($do) $f->checkDate($do, 'Špatný formát data ("Do")', 'do');
		$f->checkNumeric(post('kapacita'), 'Kapacita musí být zadána číselně', 'kapacita');
		
		return $f->isValid() ? true : $f;
    }
}
?>