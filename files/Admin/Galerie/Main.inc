<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa galerie');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'upload':
		if(empty($_POST)) {
			include('files/Admin/Galerie/Upload.inc');
			return;
		} else {
			if(is_numeric(getPostField('n'))) {
				include('files/Admin/Galerie/Upload.inc');
				return;
			}
			$dir = DBGalerie::getSingleDir(getPostField('dir') ? getPostField('dir') : 0);
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
				$fileExt = Settings::$foto_types[$files['type'][$i]];
				$pieces[count($pieces) - 1] = $fileExt;
				$fileName = implode('.', $pieces);
				$path = 'galerie/' . $dir['gd_id'] . '/' . $fileName;
				
				if(!isset($names[$i]))
					$names[$i] = $fileName;
				
				DBGalerie::addFoto($dir['gd_id'], $path, $names[$i], User::getUserID());
				$new_id = mysql_insert_id();
				
				$fileName = $new_id . '.' . $fileExt;
				$path = 'galerie/' . $dir['gd_id'] . '/' . $fileName;
				DBGalerie::editFoto($new_id, $path);
				
				if(move_uploaded_file($fileUpload, $path)) {
					chmod($path, 0666);
					$added = true;
					
					list($width, $height) = getimagesize($path);
					if(!(($width > THUMBNAIL_MAX) || ($height > THUMBNAIL_MAX))) {
						$nWidth = $width;
						$nHeight = $height;
					} else {
						$scale = ($width > $height) ? (THUMBNAIL_MAX / $width) : (THUMBNAIL_MAX / $height);
						$nWidth = round($width * $scale);
						$nHeight = round($height * $scale);	
					}
					
					$fn_suffix = Settings::$gd_function_suffix[$files['type'][$i]];
					$fn_read = 'imageCreateFrom' . $fn_suffix;
					$fn_write = 'image' . $fn_suffix;
					if(!function_exists('imagebmp') || !function_exists('imagecreatefrombmp'))
						include('files/Core/bmp.php');
										
					if($source = $fn_read($path)) {
						$thumbnail = imageCreateTruecolor($nWidth, $nHeight);

						imageCopyResized($thumbnail, $source,
						0, 0, 0, 0, $nWidth, $nHeight, $width, $height);
					}
					$fn_write($thumbnail, 'galerie/' . $dir['gd_id'] . '/tn_' . $fileName);
					imageDestroy($thumbnail);
				} else {
					DBGalerie::removeFoto($new_id);
					View::setRedirectMessage('Nepodařilo se nahrát soubor číslo ' . ($i + 1));
					
					$error = true;
				}
			}
			if(isset($error) && $error == true) {
				if(isset($added) && $added)
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil galerii "' .
						$dir['gd_name'] . '"');
				View::redirect('/admin/galerie', 'Bohužel, některé fotky se nepodařilo nahrát :o(');
			} else {
				if(isset($added) && $added)
					DBNovinky::addNovinka('Uživatel ' . User::getUserWholeName() . ' upravil galerii "' .
						$dir['gd_name'] . '"');
				View::redirect('/admin/galerie', 'Fotky přidány');
			}
		}
		break;
		
	case 'edit':
		$id = isset($action[1]) ? $action[1] : '';
		
		if(!is_numeric($id))
			View::redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
		
		if(empty($_POST)) {
			include('files/Admin/Galerie/DisplayEdit.inc');
			return;
		} else {
			switch(getPostField('action')) {
				case 'edit':
					notice('Not Implemented');
					include('files/Admin/Galerie/DisplayEdit.inc');
					return;
				case 'remove':
					$galerie = getPostField('galerie');
					if(empty($galerie)) {
						include('files/Admin/Galerie/DisplayEdit.inc');
						return;
					}
					foreach($galerie as $item) {
						$data = DBGalerie::getSingleFoto($item);
						DBGalerie::removeFoto($item);
						
						$pieces = explode('/', $data['gf_path']);
						$pieces[2] = 'tn_' . $pieces[2];
						$tn = implode('/', $pieces);
						
						unlink($data['gf_path']);
						unlink($tn);
					}
					notice('Fotky odebrány');
					
					include('files/Admin/Galerie/DisplayEdit.inc');
					return;
			}
		}
}

if(empty($_POST)) {
	include('files/Admin/Galerie/Display.inc');
	return;
}

switch(getPostField('action')) {
	case 'edit':
		$galerie = getPostField('galerie');
		if(is_numeric($galerie[0])) {
			header('Location: /admin/galerie/edit/' . $galerie[0]);
			return;
		} else {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		break;
	case 'remove':
		if(!is_array(getPostField('galerie'))) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		echo '<form action="/admin/galerie" method="POST">';
		echo 'Opravdu chcete odstranit tyto složky a jejich podsložky?<br/><br/>';
		foreach(getPostField('galerie') as $id) {
			$data = DBGalerie::getSingleDir($id);
			echo "\t", $data['gd_name'];
			echo '<br />';
			echo '<input type="hidden" name="galerie[]" value="', $id, '" />';
		}
		echo '<br/>';
		echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
		echo '</form>';
		echo '<a href="/admin/galerie">Zpět</a>';
		return;
	
	case 'remove_confirm':
		if(!is_array(getPostField('galerie'))) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		foreach(getPostField('galerie') as $id) {
			DBGalerie::removeDir($id);
			$scan = glob('galerie/' . $id . '/*');
            foreach($scan as $index => $path){
                @unlink($path);
            }
            @rmdir('galerie/' . $id);
		}
		
		notice('Složky odebrány');
		include('files/Admin/Galerie/Display.inc');
		return;
	
	case 'addir':
	default:
		if(!getPostField('name') && !getPostField('parent')) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		if(getPostField('parent') == 'none') {
			$parent = 0;
			$level = 2;
		} else {
			list($parent,$level) = explode('-', getPostField('parent'));
			$level = ++$level;
		}
		$name = getPostField('name');
		
		DBGalerie::addDir($name, $parent, $level);
		mkdir('galerie/' . mysql_insert_id());
		
		notice('Složka přidána');
		include('files/Admin/Galerie/Display.inc');
		return;
			
}
?>