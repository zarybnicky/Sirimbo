<?php
class Controller_Admin_Dokumenty implements Controller_Interface {
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Dokumenty/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$dokumenty = post('dokumenty');
				if($dokumenty[0])
					View::redirect('/admin/dokumenty/edit/' . $dokumenty[0]);
				break;
			
			case 'upload':
				if(empty($_FILES))
					break;
				$fileUpload = $_FILES['file']['tmp_name'];
				$fileName = $_FILES['file']['name'];
				$fileName = str_replace(
					array('#', '$', '%', '&', '^', '*', '?'),
					array('No.', 'Dolar', 'Procento', 'And', ''), $fileName);
				
				$path = 'upload/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);
				
				if(!post('name'))
					post('name', $fileName);
				
				if(move_uploaded_file($fileUpload, $path)) {
					chmod($path, 0666);
					DBDokumenty::addDokument($path, post('name'), $fileName,
						post('kategorie'), User::getUserID());
					notice('Soubor byl úspěšně nahrán');
					
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal dokument "' .
						post('name') . '"');
				} else {
					notice('Bohužel, zkus to znova :o(');
				}
				include('files/Admin/Dokumenty/Display.inc');
				return;
			
			case 'remove': //FIXME:URI Building
				if(!is_array(post('dokumenty')))
					break;
				$url = Request::getURI() . '/remove';
				foreach(post('dokumenty') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
	}
	function edit($id = null) {
		if(!$id || !($data = DBDokumenty::getSingleDokument($id)))
			View::redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');
		
		//TODO: Dokument - Admin - edit kategorie
		if(!empty($_POST) && post('newname')) {
			$newname = post('newname');
			
			DBDokumenty::editDokument($id, $newname);
			View::redirect('/admin/dokumenty', 'Příspěvek úspěšně upraven');
		}
		echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';
		echo 'Staré jméno:&nbsp;', $data['d_name'], '<br />';
		echo 'Nové jméno:&nbsp;';
		echo '<input type="text" name="newname" value="', $data['d_name'], '" />';
		echo '<button type="submit" name="action" value="edit_confirm">Upravit</button>';
		echo '</form><br />';
		include('files/Admin/Dokumenty/Display.inc');
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Dokumenty/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('dokumenty')))
			View::redirect('/admin/dokumenty');
		foreach(post('dokumenty') as $id) {
			$data = DBDokumenty::getSingleDokument($id);
			if(Permissions::canEditDokument($data['d_kdo'])) {
				unlink($data['d_path']);
				DBDokumenty::removeDokument($id);
			} else {
				$error = true;
			}
		}
		if(isset($error) && $error)
			View::viewError(ER_AUTHORIZATION);
		
		View::redirect('/admin/dokumenty', 'Dokumenty odebrány');
	}
}
?>