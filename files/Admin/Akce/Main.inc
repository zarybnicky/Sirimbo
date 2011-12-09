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
			$od = getPostField('od_year') . '-' . getPostField('od_month') . '-' . getPostField('od_day');
			$do = getPostField('do_year') . '-' . getPostField('do_month') . '-' . getPostField('do_day');
			
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$error = false;
			
			ob_start();
			include('files/Admin/Akce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBAkce::addAkce(getPostField('jmeno'), getPostField('kde'), getPostField('info'),
					$od, $do, getPostField('kapacita'), getPostField('dokumenty'),
					(getPostField('lock') == 'lock') ? 1 : 0);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal klubovou akci "' .
					getPostField('jmeno') . '"');
				
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
			$_POST['id'] = $id;
			$_POST['jmeno'] = $data['a_jmeno'];
			$_POST['kde'] = $data['a_kde'];
			$_POST['info'] = $data['a_info'];
			list($od_year, $od_month, $od_day) = explode('-', $data['a_od']);
			$_POST['od_year'] = $od_year;
			$_POST['od_month'] = $od_month;
			$_POST['od_day'] = $od_day;
			list($do_year, $do_month, $do_day) = explode('-', $data['a_do']);
			$_POST['do_year'] = $do_year;
			$_POST['do_month'] = $do_month;
			$_POST['do_day'] = $do_day;
			$_POST['kapacita'] = $data['a_kapacita'];
			$_POST['dokumenty'] = unserialize($data['a_dokumenty']);
			$_POST['lock'] = $data['a_lock'];
			
			include('files/Admin/Akce/Form.inc');
			return;
		} else {
			$od = getPostField('od_year') . '-' . getPostField('od_month') . '-' . getPostField('od_day');
			$do = getPostField('do_year') . '-' . getPostField('do_month') . '-' . getPostField('do_day');
			if($do == '0000-00-00' || strcmp($od, $do) > 0)
				$do = $od;
			
			$error = false;
			ob_start();
			include('files/Admin/Akce/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				DBAkce::editAkce($id, getPostField('jmeno'), getPostField('kde'), getPostField('info'),
					$od, $do, getPostField('kapacita'), getPostField('dokumenty'),
					(getPostField('lock') == 'lock') ? 1 : 0);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' změnil detaily akce "' .
					getPostField('jmeno') . '"');
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

switch(getPostField('action')) {
	
	case 'remove':
		if(is_array(getPostField('akce'))) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';
			echo 'Opravdu chcete odstranit tábor/soustředění:<br/><br/>';
			foreach(getPostField('akce') as $item) {
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
		if(is_array(getPostField("akce"))) {
			foreach(getPostField("akce") as $id) {
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
		$akce = getPostField('akce');
		if($akce[0]) {
			header('Location: /admin/akce/edit/' . $akce[0]);
			return;
		}
		break;
		
	case "edit_detail":
		$akce = getPostField("akce");
		if($akce[0]) {
			header("Location: /admin/akce/detail/" . $akce[0]);
			return;
		}
		break;
		
	case "edit_doku":
		$akce = getPostField("akce");
		if($akce[0]) {
			header("Location: /admin/akce/dokumenty/" . $akce[0]);
			return;
		}
		break;
}

include("files/Admin/Akce/Display.inc");
return;
?>f