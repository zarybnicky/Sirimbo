<?php
User::checkPermissionsError(L_EDITOR);
header_main('Správa souborů');

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if($action[0] == 'edit' && is_numeric($action[1])) {
	$id = $action[1];
	$file = DBDokumenty::getSingleDokument($id);
	
	if(!getPostField('newname')) {
		if(!User::checkPermissionsBool(L_ADMIN) && 
				User::getUserID() != $file['d_kdo'])
			View::viewError(ER_AUTHORIZATION);
		
		echo '<form action="/admin/dokumenty" method="POST">';
		echo 'Staré jméno:&nbsp;', $file['d_name'], '<br />';
		echo 'Nové jméno:&nbsp;';
		echo '<input type="text" name="newname" value="', $file['d_name'], '" />';
		echo '<input type="hidden" name="id" value="', $id, '" />';
		echo '<button type="submit" name="action" value="edit_confirm">Upravit</button>';
		echo '</form><br />';
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
	
	case 'edit_confirm':
		$id = getPostField('id');
		$newname = (getPostField('newname')) ? getPostField('newname') : '---EMPTY---';
		
		if(!User::checkPermissionsBool(L_ADMIN) && 
				User::getUserID() != DBDokumenty::getDokumentUserID($id))
			View::viewError(ER_AUTHORIZATION);
		
		DBDokumenty::editDokument($id, $newname);
		notice('Příspěvek úspěšně upraven');
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
			} else {
				notice('Bohužel, zkus to znova :o(');
			}
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
			foreach(getPostField('dokumenty') as $item) {
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserID() == DBDokumenty::getDokumentUserID($item))) {
					unlink(DBDokumenty::getDokumentPath($item));
					DBDokumenty::removeDokument($item);
				} else {
					$error = true;
				}
			}
			if($error)
				View::viewError(ER_AUTHORIZATION);
			notice('Dokumenty odebrány');
		}
		break;
}

include('files/Admin/Dokumenty/Display.inc');
return;
?>
