<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa plateb');

notice(View::getRedirectMessage());

$request = explode('?', $_SERVER['REQUEST_URI']);
$request = explode('/', $request[0]);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';	

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Platby/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Platby/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				$data = DBUser::getUserData(post('user'));
				
				$placeno = post('placeno_year') . '-' . post('placeno_month') .
					'-' . post('placeno_day');
				$placeno_date = new DateTime($placeno);
				
				$months = $data['us_platba_mesic'] ?
					floor(post('castka') / $data['us_platba_mesic']) : 0;
				if($months > 0) {
					$plati_do_date = new DateInterval('P' . $months . 'M');
					
					$plati_do = $placeno_date->add($plati_do_date)->format('Y-m-d');
				} else {
					$plati_do = $placeno;
				}
				
				DBPlatby::addPlatba(post('user'), post('castka'), $placeno, $plati_do);
				
				View::redirect('/admin/platby', 'Platba úspěšně přidána');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBPlatby::getSinglePlatba($id)))
			View::redirect('/admin/platby', 'Platba s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('user', $data['up_id_user']);
			post('castka', $data['up_castka']);
			list($placeno_year, $placeno_month, $placeno_day) =
				explode('-', $data['up_placeno']);
			post('placeno_year', $placeno_year);
			post('placeno_month', $placeno_month);
			post('placeno_day', $placeno_day);
			
			include('files/Admin/Platby/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Platby/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				$data = DBUser::getUserData(post('user'));
				
				$placeno = post('placeno_year') . '-' . post('placeno_month') .
					'-' . post('placeno_day');
				
				if(post('prepocitat')) {
					$placeno_date = new DateTime($placeno);
					
					$months = $data['us_platba_mesic'] ?
						floor(post('castka') / $data['us_platba_mesic']) : 0;
					if($months > 0) {
						$plati_do_date = new DateInterval('P' . $months . 'M');
						
						$plati_do = $placeno_date->add($plati_do_date)->format('Y-m-d');
					} else {
						$plati_do = $placeno;
					}
				} else {
					$platba = DBPlatby::getSinglePlatba($id);
					
					$plati_do = $platba['up_plati_do'];
				}
				
				DBPlatby::editPlatba($id, post('user'), post('castka'), $placeno, $plati_do);
				
				View::redirect('/admin/platby', 'Platba úspěšně upravena');
			} else {
				echo $html;
				return;
			}
		}
		break;
}

if(empty($_POST)) {
	include('files/Admin/Platby/Display.inc');
	return;
}

switch(post('action')) {
	case 'edit':
		$platby = post('platby');
		if($platby[0]) {
			header('Location: /admin/platby/edit/' . $platby[0]);
			return;
		}
		break;
	case 'remove':
		if(!is_array(post('platby'))) {
			include('files/Admin/Platby/Display.inc');
			return;
		}
		echo '<form action="/admin/platby" method="POST">';
		echo 'Opravdu chcete odstranit platby:<br/><br/>';
		foreach(post('platby') as $id) {
			$data = DBPlatby::getSinglePlatba($id);
			echo "\tod '", $data['u_prijmeni'], ', ', $data['u_jmeno'],
				'\' - placeno ', formatDate($data['up_placeno']);
			echo '<br />';
			echo '<input type="hidden" name="platby[]" value="', $id, '" />';
		}
		echo '<br/>';
		echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
		echo '</form>';
		echo '<a href="/admin/platby">Zpět</a>';
		return;

	case 'remove_confirm':
		if(!is_array(post('platby'))) {
			include('files/Admin/Platby/Display.inc');
			return;
		}
		foreach(post('platby') as $id) {
			DBPlatby::removePlatba($id);
		}
		
		notice('Platby odebrány');
		include('files/Admin/Platby/Display.inc');
		return;
}

include('files/Admin/Platby/Display.inc');
?>