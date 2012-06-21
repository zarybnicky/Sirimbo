<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa akcí');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Akce/Form.inc');
			return;
		} else {
			$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
			$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$error = false;
			
			ob_start();
			include('files/Admin/Akce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBAkce::addAkce(post('jmeno'), post('kde'), post('info'),
					$od, $do, post('kapacita'), post('dokumenty'),
					(post('lock') == 'lock') ? 1 : 0);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal klubovou akci "' .
					post('jmeno') . '"');
				
				View::redirect('/admin/akce', 'Akce přidána');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBAkce::getSingleAkce($id)))
			View::redirect('/admin/akce', 'Akce s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('id', $id);
			post('jmeno', $data['a_jmeno']);
			post('kde', $data['a_kde']);
			post('info', $data['a_info']);
			list($od_year, $od_month, $od_day) = explode('-', $data['a_od']);
			post('od_year', $od_year);
			post('od_month', $od_month);
			post('od_day', $od_day);
			list($do_year, $do_month, $do_day) = explode('-', $data['a_do']);
			post('do_year', $do_year);
			post('do_month', $do_month);
			post('do_day', $do_day);
			post('kapacita', $data['a_kapacita']);
			post('dokumenty', unserialize($data['a_dokumenty']));
			post('lock', $data['a_lock']);
			
			include('files/Admin/Akce/Form.inc');
			return;
		} else {
			$od = post('od_year') . '-' . post('od_month') . '-' . post('od_day');
			$do = post('do_year') . '-' . post('do_month') . '-' . post('do_day');
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$error = false;
			ob_start();
			include('files/Admin/Akce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBAkce::editAkce($id, post('jmeno'), post('kde'), post('info'),
					$od, $do, post('kapacita'), post('dokumenty'),
					(post('lock') == 'lock') ? 1 : 0);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' změnil detaily akce "' .
					post('jmeno') . '"');
				View::redirect('/admin/akce', 'Akce upravena');
			} else {
				echo $html;
				return;
			}
		}
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
?>f