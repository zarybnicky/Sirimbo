<?php
User::checkPermissionsError(L_ADMIN);

header_main('Správa plateb');
notice(View::getRedirectMessage());

switch(Request::getAction()) {
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
				$platby = DBPlatby::getPlatbyFromUser(post('user'));
				
				$placeno = Helper::get()
					->date()->name('placeno')
					->getPost();
				$obdobi = post('obdobi');
				list($year, $month, $day) = explode('-', $placeno);
				
				if(strcmp($placeno, $year . '-06-30') < 0 &&
						($obdobi == '1-pololeti' || $obdobi == '1-ctvrtleti' ||
						$obdobi == '2-ctvrtleti')) {
					$year = (string) (((int) $year) - 1);
				} elseif(strcmp($placeno, $year . '-07-01') >= 0 &&
						($obdobi == '2-pololeti' || $obdobi == '13-ctvrtleti' ||
						$obdobi == '4-ctvrtleti')) {
					$year = (string) (((int) $year) + 1);
				}
				$plati_do = $year . Settings::$platby_obdobi[$obdobi][1];
				
				foreach($platby as $platba) {
					if($platba['up_obdobi'] != $obdobi)
						continue;
					
					DBPlatby::editPlatba($platba['up_id'], post('user'), $obdobi,
						post('castka') + $platba['up_castka'], $placeno,
						$plati_do);
					
					View::redirect(post('referer') ? post('referer') : '/admin/platby',
						'Druhá platba za stejné období - sloučeno!');
				}
				
				DBPlatby::addPlatba(post('user'), $obdobi, post('castka'), $placeno, $plati_do);
				
				View::redirect(post('referer') ? post('referer') : '/admin/platby',
					'Platba úspěšně přidána');
			} else {
				echo $html;
				return;
			}
		}
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !is_numeric($id) || !($data = DBPlatby::getSinglePlatba($id)))
			View::redirect(post('referer') ? post('referer') : '/admin/platby',
				'Platba s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('user', $data['up_id_user']);
			post('castka', $data['up_castka']);
			post('obdobi', $data['up_obdobi']);
			post('placeno', $data['up_placeno']);
			
			include('files/Admin/Platby/Form.inc');
			return;
		} else {
			$error = false;
			ob_start();
			include('files/Admin/Platby/Form.inc');	
			$html = ob_get_clean();
			
			if(!$error) {
				$data = DBUser::getUserData(post('user'));
				$platby = DBPlatby::getPlatbyFromUser(post('user'));
				
				$placeno = Helper::get()->date()->name('placeno')->getPost();
				$obdobi = post('obdobi');
				list($year, $month, $day) = explode('-', $placeno);
				
				if(strcmp($placeno, $year . '-06-30') < 0 &&
						$obdobi == '1-ctvrtleti') {
					$year = (string) (((int) $year) - 1);
				} elseif(strcmp($placeno, $year . '-07-01') >= 0 &&
						($obdobi == '1-pololeti' || $obdobi == '2-pololeti' ||
						$obdobi == '2-ctvrtleti' || $obdobi == '3-ctvrtleti' ||
						$obdobi == '4-ctvrtleti')) {
					$year = (string) (((int) $year) + 1);
				}
				$plati_do = $year . Settings::$platby_obdobi[$obdobi][1];
			
				foreach($platby as $platba) {
					if($platba['up_obdobi'] != $obdobi)
						continue;
					
					DBPlatby::editPlatba($platba['up_id'], post('user'), $obdobi,
						post('castka') + $platba['up_castka'], $placeno,
						$plati_do);
					DBPlatby::removePlatba($id);
					
					View::redirect(post('referer') ? post('referer') : '/admin/platby',
						'Druhá platba za stejné období - sloučeno!');
				}
				
				DBPlatby::editPlatba($id, post('user'), $obdobi, post('castka'),
					$placeno, $plati_do);
				
				View::redirect(post('referer') ? post('referer') : '/admin/platby',
					'Platba úspěšně upravena');
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