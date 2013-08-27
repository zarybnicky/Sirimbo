<?php
include_once('files/Controller/Admin.php');
class Controller_Admin_Dokumenty extends Controller_Admin {
	function __construct() {
		Permissions::checkError('dokumenty', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			$this->render('files/Admin/Dokumenty/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$dokumenty = post('dokumenty');
				if($dokumenty[0])
					$this->redirect('/admin/dokumenty/edit/' . $dokumenty[0]);
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
				$this->render('files/Admin/Dokumenty/Display.inc');
				return;
			
			case 'remove':
				if(!is_array(post('dokumenty')))
					break;
				$url = '/admin/dokumenty/remove?';
				foreach(post('dokumenty') as $id)
					$url .= '&u[]=' . $id;
				$this->redirect($url);
				break;
		}
	}
	function edit($id = null) {
		if(!$id || !($data = DBDokumenty::getSingleDokument($id)))
			$this->redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');
	
		if(!empty($_POST) && post('newname')) {
			$newname = post('newname');
			
			DBDokumenty::editDokument($id, $newname);
			$this->redirect('/admin/dokumenty', 'Příspěvek úspěšně upraven');
		}
		echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';
		echo 'Staré jméno:&nbsp;', $data['d_name'], '<br />';
		echo 'Nové jméno:&nbsp;';
		echo '<input type="text" name="newname" value="', $data['d_name'], '" />';
		echo '<button type="submit" name="action" value="edit_confirm">Upravit</button>';
		echo '</form><br />';
		$this->render('files/Admin/Dokumenty/Display.inc');
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			$this->render('files/Admin/Dokumenty/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('dokumenty')))
			$this->redirect('/admin/dokumenty');
		foreach(post('dokumenty') as $id) {
			$data = DBDokumenty::getSingleDokument($id);
			if(Permissions::check('dokumenty', P_OWNED, $data['d_kdo'])) {
				unlink($data['d_path']);
				DBDokumenty::removeDokument($id);
			} else {
				$error = true;
			}
		}
		if(isset($error) && $error)
			throw new Exception("Máte nedostatečnou autorizaci pro tuto akci!");
		
		$this->redirect('/admin/dokumenty', 'Dokumenty odebrány');
	}
}
?>