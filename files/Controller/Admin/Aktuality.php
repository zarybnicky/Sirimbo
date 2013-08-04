<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Aktuality extends Controller_Admin {
	function __construct() {
		Permissions::checkError('aktuality', P_OWNED);
	}
	function view($id = null) {
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
					break;
				$url = '/admin/aktuality/remove?';
				foreach(post('aktuality') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		include("files/Admin/Aktuality/Display.inc");
	}
	function add($id = null) {
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
	}
	function edit($id = null) {
		if(!$id || !($data = DBAktuality::getSingleAktualita($id)))
			View::redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
		
		Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);
		
		if(empty($_POST)) {
			post('kat', $data['at_kat']);
			post("jmeno", $data["at_jmeno"]);
			post("text", stripslashes($data["at_text"]));
			post('foto', $data['at_foto']);
			
			include("files/Admin/Aktuality/Form.inc");
			return;
		}
		if(post('kat') != $data['at_kat'] || post('jmeno') != $data['at_jmeno'] ||
				post('text') != $data['at_text'] || post('foto') != $data['at_foto']) {
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
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Aktuality/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('aktuality')))
			View::redirect('/admin/aktuality');
		foreach(post('aktuality') as $id) {
			$data = DBAktuality::getSingleAktualita($id);
			
			if(Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
				DBAktuality::removeAktualita($id);
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' vymazal článek ' .
					$data['at_jmeno']);
			} else {
				$error = true;
			}
		}
		if(isset($error) && $error)
			View::viewError(ER_AUTHORIZATION);
		
		View::redirect('/admin/aktuality', 'Články odebrány');
    }
}
?>