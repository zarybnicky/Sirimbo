<?php
User::checkPermissionsError(L_EDITOR);
header_main("Správa aktualit");

notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Aktuality/Form.inc');
			return;
		}
		$preview = trim(substr(strip_tags(post('text')), 0, AKTUALITY_PREVIEW));
		
		if(($foto_data = DBGalerie::getFotky(post('foto'))) &&
				$foto_data[0]['gf_id'])
			$f_id = $foto_data[0]['gf_id'];
		else
			$f_id = 0;
		
		DBAktuality::addAktualita(User::getUserID(), post('kat'), post('jmeno'),
			post('text'), $preview, post('foto'), $f_id);
		DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal článek ' .
			post('jmeno'));
		View::redirect('/admin/aktuality', 'Článek přidán');
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBAktuality::getSingleAktualita($id)))
			View::redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
		
		if(!Permissions::canEditAktualita($data['at_kdo']))
			View::viewError(ER_AUTHORIZATION);
		
		if(empty($_POST)) {
			post('kat', $data['at_kat']);
			post("jmeno", $data["at_jmeno"]);
			post("text", stripslashes($data["at_text"]));
			post('foto', $data['at_foto']);
			
			include("files/Admin/Aktuality/Form.inc");
			return;
		}
		if(post('kat') != $data['at_kat'] ||
				post('jmeno') != $data['at_jmeno'] ||
				post('text') != $data['at_text'] ||
				post('foto') != $data['at_foto']) {
			$preview = trim(substr(strip_tags(post('text')), 0, AKTUALITY_PREVIEW));
			
			if(($foto_data = DBGalerie::getFotky(post('foto'))) &&
					$foto_data[0]['gf_id'])
				$f_id = $foto_data[0]['gf_id'];
			else
				$f_id = 0;
			
			DBAktuality::editAktualita($id, post('kat'), post('jmeno'), post('text'),
				$preview, post('foto'), $f_id);
			$changed = true;
		}
		if(isset($changed) && $changed) {
			DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil článek ' .
				post('jmeno'));
		}
		
		View::redirect('/admin/aktuality', 'Článek změněn');
		return;
}

if(empty($_POST)) {
	include("files/Admin/Aktuality/Display.inc");
	return;
}

switch(post("action")) {
	case 'edit':
		$aktuality = post('aktuality');
		if($aktuality[0])
			View::redirect('/admin/aktuality/edit/' . $aktuality[0]);
		break;
		
	case 'foto':
		$aktuality = post('aktuality');
		if($aktuality[0])
			View::redirect('/admin/aktuality/foto/' . $aktuality[0]);
		break;

	case 'remove':
		if(!is_array(post('aktuality')))
			break;;
		
		echo '<form action="/admin/aktuality" method="post">';
		echo 'Opravdu chcete odstranit články:<br/><br/>';
		foreach(post('aktuality') as $id) {
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

	case 'remove_confirm':
		if(!is_array(post("aktuality")))
			break;
		foreach(post("aktuality") as $item) {
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
		break;
}
include("files/Admin/Aktuality/Display.inc");
return;
?>