<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa galerie');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

if(!$action[0]) {
	include('files/Admin/Galerie/Display.inc');
	return;
}
switch($action[0]) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Galerie/Add.inc');
			return;
		} else {
			$dir = DBGalerie::getSingleDir(getPostField('dir') ? getPostField('dir') : 1);
			$files = $_FILES['file'];
			$names = getPostField('name');
			
			$count = count($files['name']);
			for($i = 0; $i < $count; $i++) {
				if($files['error'][$i] > 0) {
					View::setRedirectMessage('Nepodařilo se nahrát soubor číslo ' . ($i + 1));
					continue;
				}
				
				if(!array_key_exists($files['type'][$i], Settings::$foto_types)) {
					View::setRedirectMessage('Soubor číslo ' . ($i + 1) .
						' není fotka podporovaného typu a byl přeskočen');
					continue;
				}
				
				$fileUpload = $files['tmp_name'][$i];
				$fileName = $files['name'][$i];
				$fileName = str_replace(
					array('#', '$', '%', '&', '^', '*', '?'),
					array('No.', 'Dolar', 'Procento', 'And', ''), $fileName
				);
				$pieces = explode('.', $fileName);
				$pieces[count($pieces) - 1] = Settings::$foto_types[$files['type'][$i]];
				$fileName = implode('.', $pieces);
				
				if(!isset($names[$i]))
					$_POST['name'] = $fileName;
				
				if(empty($dir))
					$dir['gd_id'] = 0;
				
				$path = 'galerie/' . $dir['gd_id'] . '/' . $fileName;
				
				if(move_uploaded_file($fileUpload, $path)) {
					chmod($path, 0666);
					
					DBGalerie::addFoto($dir['gd_id'], $path, $fileName, User::getUserID());
					
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil galerii "' .
						$dir['gd_name'] . '"');
				} else {
					$error = true;
				}
			}
			if(isset($error) && $error == true) {
				View::redirect('/admin/galerie', 'Bohužel, některé fotky se nepodařilo nahrát :o(');
			} else {
				View::redirect('/admin/galerie', 'Fotky přidány');
			}
		}
		break;

	case 'editdirs':
		if(empty($_POST)) {
			include('files/Admin/Galerie/EditDirs.inc');
		}
		break;
}
?>