<?php
User::checkPermissionsError(L_EDITOR);
header_main("Správa anket");

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Ankety/Form.inc');
			return;
		} else {
			$visible = (bool) getPostField("visible");
			if(User::checkPermissionsBool(L_EDITOR) && !User::checkPermissionsBool(L_ADMIN)) {
				$visible = false;
				View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
			}
			
			DBAnkety::addAnketa(User::getUserID(), getPostField('jmeno'), getPostField('text'), '0',
				(bool) $visible);
			$data = DBAnkety::getLatestAnketa();
			
			if($visible) {
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal anketu ' .
					getPostField('jmeno'));
			}
		
			if(getPostField("add_text")) {
				DBAnkety::addAnketaItem($data['ak_id'], getPostField("add_text"));
				unset($_POST["add_text"]);
			}
			View::redirect('/admin/ankety/edit/' . $data['ak_id'], 'Anketa přidána');
		}
		break;
	
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!$id || !is_numeric($id) || !($data = DBAnkety::getSingleAnketa($id)))
			View::redirect('/admin/ankety', 'Anketa s takovým ID neexistuje');
		
		if(!Permissions::canEditAnketa($data['ak_kdo']))
			View::viewError(ER_AUTHORIZATION);
			
		$items = DBAnkety::getAnketaItems($id);
			
		if(empty($_POST)) {
			$_POST["jmeno"] = $data["ak_jmeno"];
			$_POST["text"] = $data["ak_text"];
			$_POST["visible"] = $data["ak_visible"];
			
			include("files/Admin/Ankety/Form.inc");
			return;
		} else {
			$changed = false;
			
			//-----Odebrání položky-----//
			if(getPostField("remove") > 0) {
				DBAnkety::removeAnketaItem(getPostField("remove"));
				$items = DBAnkety::getAnketaItems($id);
				$changed = true;
			}
			
			//-----Jednotlivé položky-----//
			foreach($items as $item) {
				if(getPostField($item["aki_id"] . "-text") != $item["aki_text"]) {
					DBAnkety::editAnketaItem($item["aki_id"], getPostField($item["aki_id"] . "-text"));
					$changed = true;
				}
			}
			$items = DBAnkety::getAnketaItems($id);
			
			//-----Přidávání položek-----//
			if(getPostField("add_text")) {
				DBAnkety::addAnketaItem($id, getPostField("add_text"));
				unset($_POST["add_text"]);
				$items = DBAnkety::getAnketaItems($id);
				$changed = true;
			}
			
			$visible = (bool) getPostField('visible');
			$visible_prev = $data['ak_visible'];
			if(User::checkPermissionsBool(L_EDITOR) && !User::checkPermissionsBool(L_ADMIN) &&
					$visible != $visible_prev) {
				$visible = $visible_prev;
				View::setRedirectMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
			}
			
			if($visible != $visible_prev ||
					getPostField('jmeno') != $data['ak_jmeno'] ||
					getPostField('text') != $data['ak_text']) {
				DBAnkety::editAnketa($id, getPostField('jmeno'), getPostField('text'), '0', $visible);
				$data = DBAnkety::getSingleAnketa($id);
				$changed = true;
			}
			if($changed) {
				if($visible) {
					if(!$visible_prev)
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal anketu ' .
							getPostField('jmeno'));
					else
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil anketu ' .
							$data['ak_jmeno']);
				} elseif(!$visible && $visible_prev) {
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil anketu ' .
							$data['ak_jmeno']);
				}
			}
			
			$_POST["jmeno"] = $data["ak_jmeno"];
			$_POST["text"] = $data["ak_text"];
			$_POST["visible"] = $data["ak_visible"];
			
			include("files/Admin/Ankety/Form.inc");
			return;
		}
		break;
}
//TODO: klonování N,R
//TODO: disable buttons where I can't edit

if(empty($_POST)) {
	include("files/Admin/Ankety/Display.inc");
	return;
}

switch(getPostField("action")) {
	case 'edit':
		$ankety = getPostField('ankety');
		if($ankety[0]) {
			header('Location: /admin/ankety/edit/' . $ankety[0]);
			return;
		}
		break;

	case 'remove':
		if(is_array(getPostField('ankety'))) {
			echo '<form action="/admin/ankety" method="POST">';
			echo 'Opravdu chcete odstranit ankety:<br/><br/>';
			foreach(getPostField('ankety') as $id) {
				$data = DBAnkety::getSingleAnketa($id);
				echo "\t", $data['ak_jmeno'];
				echo '<br />';
				echo '<input type="hidden" name=ankety[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/ankety">Zpět</a>';
			return;
		}
		break;

	case 'remove_confirm':
		if(is_array(getPostField("ankety"))) {
			foreach(getPostField("ankety") as $item) {
				$data = DBAnkety::getSingleAnketa($item);
				
				if(Permissions::canEditAnketa($data['ak_kdo'])) {
					DBAnkety::removeAnketa($item);
					if($data['ak_visible'])
						DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' zrušil anketu ' .
							$data['ak_jmeno']);
				} else {
					$error = true;
				}
			}
			if(isset($error) && $error)
				View::viewError(ER_AUTHORIZATION);
			
			notice("Ankety odebrány");
			include("files/Admin/Ankety/Display.inc");
			return;
		}
		break;
}
include("files/Admin/Ankety/Display.inc");
return;
?>