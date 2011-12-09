<?php
User::checkPermissionsError(L_EDITOR);
header_main("Správa aktualit");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Aktuality/Form.inc');
			return;
		} else {
			$matches = array();
			$preview = substr(getPostField('text'), 0, AKTUALITY_PREVIEW);
			preg_match("/(.+)\s.*/i", $preview, $matches);
			
			DBAktuality::addAktualita(User::getUserID(), getPostField('kat'), getPostField('jmeno'),
				getPostField('text'), $matches[1], serialize(array()));
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal článek ' .
				getPostField('jmeno'));
			
			View::redirect('/admin/aktuality', 'Článek přidán');
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBAktuality::getSingleAktualita($id)))
			View::redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
		
		if(!Permissions::canEditAktualita($data['at_kdo']))
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
			$_POST['kat'] = $data['at_kat'];
			$_POST["jmeno"] = $data["at_jmeno"];
			$_POST["text"] = $data["at_text"];
			
			include("files/Admin/Aktuality/Form.inc");
			return;
		} else {
			if(getPostField('kat') != $data['at_kat'] ||
					getPostField('jmeno') != $data['at_jmeno'] ||
					getPostField('text') != $data['at_text']) {
				$matches = array();
				$preview = substr(getPostField('text'), 0, AKTUALITY_PREVIEW);
				preg_match("/(.+)\s.*/i", $preview, $matches);
				
				DBAktuality::editAktualita($id,getPostField('kat'), getPostField('jmeno'), getPostField('text'),
					$matches[1], serialize(array()));
				$changed = true;
			}
			if(isset($changed) && $changed) {
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil článek ' .
					getPostField('jmeno'));
			}
			
			View::redirect('/admin/aktuality', 'Článek změněn');
			return;
		}
		break;
}

if(empty($_POST)) {
	include("files/Admin/Aktuality/Display.inc");
	return;
}

switch(getPostField("action")) {
	case 'edit':
		$aktuality = getPostField('aktuality');
		if($aktuality[0]) {
			header('Location: /admin/aktuality/edit/' . $aktuality[0]);
			return;
		}
		break;

	case 'remove':
		if(is_array(getPostField('aktuality'))) {
			echo '<form action="/admin/aktuality" method="post">';
			echo 'Opravdu chcete odstranit články:<br/><br/>';
			foreach(getPostField('aktuality') as $id) {
				$data = DBAktuality::getSingleAktualita($id);
				echo "\t", $data['at_jmeno'];
				echo '<br />';
				echo '<input type="hidden" name=aktuality[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/aktuality">Zpět</a>';
			return;
		}
		break;

	case 'remove_confirm':
		if(is_array(getPostField("aktuality"))) {
			foreach(getPostField("aktuality") as $item) {
				$data = DBAktuality::getSingleAktualita($item);
				
				if(Permissions::canEditAktualita($data['at_kdo'])) {
					DBAktuality::removeAktualita($item);
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' vymazal článek ' .
						$data['at_jmeno']);
				} else {
					$error = true;
				}
			}
			if(isset($error) && $error)
				View::viewError(ER_AUTHORIZATION);
			
			notice("Články odebrány");
			include("files/Admin/Aktuality/Display.inc");
			return;
		}
		break;
}
include("files/Admin/Aktuality/Display.inc");
return;
?>