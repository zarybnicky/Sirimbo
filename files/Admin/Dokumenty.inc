<?php
User::checkPermissionsError(L_EDITOR);
header_main('Správa souborů');

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit') {
	$id = isset($action[1]) ? $action[1] : '';
	
	if(!$id || !is_numeric($id) || !($data = DBDokumenty::getSingleDokument($id)))
		View::redirect('/admin/dokumenty', 'Dokument s takovým ID neexistuje');
	
	if(!Permissions::canEditDokument($data['d_kdo']))
		View::viewError(ER_AUTHORIZATION);
	
	if(empty($_POST)) {
		echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';
		echo 'Staré jméno:&nbsp;', $data['d_name'], '<br />';
		echo 'Nové jméno:&nbsp;';
		echo '<input type="text" name="newname" value="', $data['d_name'], '" />';
		echo '<button type="submit" name="action" value="edit_confirm">Upravit</button>';
		echo '</form><br />';
	} else {
		$newname = (getPostField('newname')) ? getPostField('newname') : '---EMPTY---';
		
		DBDokumenty::editDokument($id, $newname);
		View::redirect('/admin/dokumenty', 'Příspěvek úspěšně upraven');
	}
}

if(empty($_POST)) {
	include('files/Admin/Dokumenty/Display.inc');
	return;
}

switch(getPostField('action')) {
	case 'edit':
		$dokumenty = getPostField('dokumenty');
		if($dokumenty[0]) {
			header('Location: /admin/dokumenty/edit/' . $dokumenty[0]);
			return;
		}
		break;
	
	case 'upload':
		if(!empty($_FILES)) {
			$fileUpload = $_FILES['file']['tmp_name'];
			$fileName = $_FILES['file']['name'];
			$fileName = str_replace(
				array('#', '$', '%', '&', '^', '*', '?'),
				array('No.', 'Dolar', 'Procento', 'And', ''), $fileName);
			
			$path = 'upload/' . time() . '.' . pathinfo($fileName, PATHINFO_EXTENSION);
			
			if(!getPostField('name'))
				$_POST['name'] = $fileName;
			
			if(move_uploaded_file($fileUpload, $path)) {
				chmod($path, 0666);
				DBDokumenty::addDokument($path, getPostField('name'), $fileName,
					getPostField('kategorie'), User::getUserID());
				notice('Zmákli jsme to :o)');
				
				DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' přidal dokument "' .
					getPostField('name') . '"');
			} else {
				notice('Bohužel, zkus to znova :o(');
			}
			include('files/Admin/Dokumenty/Display.inc');
			return;
		}
		break;

	case 'remove':
		if(is_array(getPostField('dokumenty'))) {
			echo '<form action="', $_SERVER['REQUEST_URI'], '" method="POST">';
			echo 'Opravdu chcete odstranit dokumenty:<br/><br/>';
			foreach(getPostField('dokumenty') as $id) {
				$name = DBDokumenty::getDokumentName($id);
				echo $name, '<br />';
				echo '<input type="hidden" name="dokumenty[]" value="', $id, '" />';
			}
			echo '<br/>';
			echo '<button type="submit" name="action" value="remove_confirm">',
				'Odstranit</button>';
			echo '</form>';
			echo '<a href="/admin/dokumenty">Zpět</button>';
			return;
		}
		break;
	
	case 'remove_confirm':
		if(is_array(getPostField('dokumenty'))) {
			foreach(getPostField('dokumenty') as $id) {
				$data = DBDokumenty::getSingleDokument($id);
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserID() == $data['d_kdo'])) {
					unlink($data['d_path']);
					DBDokumenty::removeDokument($id);
				} else {
					$error = true;
				}
			}
			if(isset($error) && $error)
				View::viewError(ER_AUTHORIZATION);
			
			notice('Dokumenty odebrány');
			include('files/Admin/Dokumenty/Display.inc');
			return;
		}
		break;
}
?>
