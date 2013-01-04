<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa akcí');

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Akce/Form.inc');
			return;
		}
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('jmeno'), 1, 255, 'Špatná délka jména akce', 'jmeno');
		$f->checkLength(post('kde'), 1, 255, 'Špatná délka místa konání', 'kde');
		$f->checkDate($od, 'Špatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Špatný formát data ("Do")', 'do');
		$f->checkNumeric(post('kapacita'), 'Kapacita musí být zadána číselně', 'kapacita');
		if(!$f->isValid()) {
			include('files/Admin/Akce/Form.inc');
			return;
		}
		DBAkce::addAkce(post('jmeno'), post('kde'), post('info'),
			$od, $do, post('kapacita'), post('dokumenty'),
			(post('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal klubovou akci "' .
			post('jmeno') . '"');
		View::redirect('/admin/akce', 'Akce přidána');
		break;
	
	case 'edit':
		$id = Request::getID();
		
		if(!$id || !($data = DBAkce::getSingleAkce($id)))
			View::redirect('/admin/akce', 'Akce s takovým ID neexistuje' . var_export($id, true));
		
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
		$od = Helper::get()->date()->name('od')->getPost();
		$do = Helper::get()->date()->name('do')->getPost();
		if(!$do || strcmp($od, $do) > 0)
			$do = $od;
		
		$f = new Form();
		$f->checkLength(post('jmeno'), 1, 255, 'Špatná délka jména akce', 'jmeno');
		$f->checkLength(post('kde'), 1, 255, 'Špatná délka místa konání', 'kde');
		$f->checkDate($od, 'Špatný formát data ("Od")', 'od');
		$f->checkDate($do, 'Špatný formát data ("Do")', 'do');
		$f->checkNumeric(post('kapacita'), 'Kapacita musí být zadána číselně', 'kapacita');
		if(!$f->isValid()) {
			include('files/Admin/Akce/Form.inc');
			return;
		}
		DBAkce::editAkce($id, post('jmeno'), post('kde'), post('info'),
			$od, $do, post('kapacita'), post('dokumenty'),
			(post('lock') == 'lock') ? 1 : 0);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' změnil detaily akce "' .
			post('jmeno') . '"');
		View::redirect('/admin/akce', 'Akce upravena');
		break;
}

if(empty($_POST)) {
	include('files/Admin/Akce/Display.inc');
	return;
}

switch(post('action')) {
	case 'remove':
		if(is_array(post('akce'))) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';
			echo 'Opravdu chcete odstranit tábor/soustředění:<br/><br/>';
			foreach(post('akce') as $item) {
				echo DBAkce::getAkceName($item) . '<br />';
				echo '<input type="hidden" name="akce[]" value="', $item, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">',
				'Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/akce">Zpět</a>';
			return;
		}
		break;
	
	case "remove_confirm":
		if(is_array(post("akce"))) {
			foreach(post("akce") as $id) {
				if(User::checkPermissionsBool(L_ADMIN)) {
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
			
			notice("Akce odebrány");
		}
		break;
	
	case "edit":
		$akce = post('akce');
		if($akce[0]) {
			header('Location: /admin/akce/edit/' . $akce[0]);
			return;
		}
		break;
		
	case "edit_detail":
		$akce = post("akce");
		if($akce[0]) {
			header("Location: /admin/akce/detail/" . $akce[0]);
			return;
		}
		break;
		
	case "edit_doku":
		$akce = post("akce");
		if($akce[0]) {
			header("Location: /admin/akce/dokumenty/" . $akce[0]);
			return;
		}
		break;
}

include("files/Admin/Akce/Display.inc");
return;
?>