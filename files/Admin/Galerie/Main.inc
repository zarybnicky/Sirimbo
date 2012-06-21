<?php
User::checkPermissionsError(L_ADMIN);
header_main('Správa galerie');

notice(View::getRedirectMessage());

$request = explode('/', $_SERVER['REQUEST_URI']);
$action = array_slice($request, 3);
if(empty($action)) $action[] = '';

switch($action[0]) {
	case 'scan':
		$db_dirs = DBGalerie::getDirs(1, 1);
		$db_files = DBGalerie::getFotky();
		$db_tmp_dirs = array();
		$db_out_dirs = array();
		$db_out_files = array();
		
		foreach($db_dirs as $dir) {
			$db_out_dirs[] = $dir['gd_path'];
		}
		foreach($db_files as $file) {
			$db_out_files[] = $file['gf_path'];
		}
		
		function recursiveDirs($dir_name, &$out_dirs, &$out_files) {
			$dirs = array();
			$files = array();
			$file_list = scandir($dir_name);
			
			foreach($file_list as $key => $file) {
				if(in_array($file, array('.','..','thumbnails'))) {
					unset($file_list[$key]);
					continue;
				}
				$file_list[$key] = $dir_name . '/' . $file;
								
				if(is_dir($file_list[$key])) {
					$out_dirs[] = $file_list[$key];
					recursiveDirs($file_list[$key], $out_dirs, $out_files);
				} else if(!strstr($file_list[$key], '/tn_')) {
					$out_files[] = $file_list[$key];
				}
			}
			array_merge($out_dirs, $dirs);
		}
		
		$file_map = array();
		$fs_dirs = array();
		$fs_files = array();
		recursiveDirs("./galerie", $fs_dirs, $fs_files);

		foreach($fs_dirs as $key1 => $needle) {
			$key2 = array_search($needle, $db_out_dirs);
			if($key2 === false)
				continue;
			unset($fs_dirs[$key1]);
			unset($db_out_dirs[$key2]);
		}
		foreach($fs_files as $key1 => $needle) {
			$key2 = array_search($needle, $db_out_files);
			if($key2 === false)
				continue;
			unset($fs_files[$key1]);
			unset($db_out_files[$key2]);
		}
		
		foreach($fs_dirs as $dir) {
			$parts = explode('/', $dir);
			$name = array_pop($parts);
			$parent = implode('/', $parts);
			
			$level = count($parts);
			
			$tn_dir = str_replace("./galerie/", './galerie/thumbnails/', $dir);
			if(!is_dir($tn_dir))
				mkdir($tn_dir);
			
			if($parent == "./galerie")
				$parent = './galerie/0';
			
			DBGalerie::addDirByPath($name, $parent, $level, $dir);
		}
		
		foreach($fs_files as $key => $file) {
			$filetype = image_type_to_mime_type(exif_imagetype($file));
			if(!$filetype || !array_key_exists($filetype, Settings::$foto_types)) {
				unset($fs_files[$key]);
				unlink($file);
				continue;
			}
			
			list($width, $height) = getimagesize($file);
			if(!(($width > THUMBNAIL_MAX) || ($height > THUMBNAIL_MAX))) {
				$nWidth = $width;
				$nHeight = $height;
			} else {
				$scale = ($width > $height) ? (THUMBNAIL_MAX / $width) : (THUMBNAIL_MAX / $height);
				$nWidth = round($width * $scale);
				$nHeight = round($height * $scale);	
			}
			
			$fn_suffix = Settings::$gd_function_suffix[$filetype];
			$fn_read = 'imageCreateFrom' . $fn_suffix;
			$fn_write = 'image' . $fn_suffix;
			if(!function_exists('imagebmp') || !function_exists('imagecreatefrombmp'))
				include('files/Core/bmp.php');
								
			if($source = $fn_read($file)) {
				$thumbnail = imageCreateTruecolor($nWidth, $nHeight);

				imageCopyResized($thumbnail, $source,
				0, 0, 0, 0, $nWidth, $nHeight, $width, $height);
			}
			$tn_path = str_replace('./galerie', './galerie/thumbnails', $file);
			$fn_write($thumbnail, $tn_path);
			imageDestroy($thumbnail);
			
			$parts = explode('/', $file);
			$name = array_pop($parts);
			$dir = implode('/', $parts);
			
			if($dir == "./galerie")
				$dir = './galerie/0';
			
			DBGalerie::addFotoByPath($dir, $file, $name, User::getUserID());
		}
		
		foreach($db_out_dirs as $dir) {
			DBGalerie::removeDirByPath($dir);
		}
		foreach($db_out_files as $files) {
			DBGalerie::removeFotoByPath($file);
		}
		
		echo 'Složek přidáno: ', count($fs_dirs), '<br/>',
			'Souborů přidáno: ', count($fs_files), '<br/>',
			'<br/>',
			'Složek odebráno: ', count($db_out_dirs), '<br/>',
			'Souborů odebráno: ', count($db_out_files), '<br/>';
		break;
		
	case 'upload':
		if(empty($_POST)) {
			include('files/Admin/Galerie/Upload.inc');
			return;
		} else {
			$dir = DBGalerie::getSingleDir(post('dir') ? post('dir') : 0);
			$files = $_FILES['file'];
			$names = post('name');
			
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
				$path = $dir['gd_path'] . '/' . $fileName;
				
				if(is_file($path)) {
					View::setRedirectMessage('Soubor číslo ' . ($i + 1) .
						' nebo soubor se stejným názvem už existuje.');
					continue;
				}
				
				if(!isset($names[$i]))
					$names[$i] = $fileName;
				
				DBGalerie::addFoto($dir['gd_id'], $path, $names[$i], User::getUserID());
				
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
					$fn_write($thumbnail, str_replace('./galerie', './galerie/thumbnails', $path));
					imageDestroy($thumbnail);
				} else {
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
			switch(post('action')) {
				case 'edit':
					notice('Not Implemented');
					include('files/Admin/Galerie/DisplayEdit.inc');
					return;
				case 'remove':
					$galerie = post('galerie');
					if(empty($galerie)) {
						include('files/Admin/Galerie/DisplayEdit.inc');
						return;
					}
					foreach($galerie as $item) {
						$data = DBGalerie::getSingleFoto($item);
						DBGalerie::removeFoto($item);
						
						unlink($data['gf_path']);
						unlink(str_replace('./galerie', './galerie/thumbnails', $data['gd_path']));
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

switch(post('action')) {
	case 'edit':
		$galerie = post('galerie');
		if(is_numeric($galerie[0])) {
			header('Location: /admin/galerie/edit/' . $galerie[0]);
			return;
		} else {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		break;
	case 'remove':
		if(!is_array(post('galerie'))) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		echo '<form action="/admin/galerie" method="POST">';
		echo 'Opravdu chcete odstranit tyto složky a jejich podsložky?<br/><br/>';
		foreach(post('galerie') as $id) {
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
		if(!is_array(post('galerie'))) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		foreach(post('galerie') as $id) {
			$data = DBGalerie::getSingleDir($id);
			DBGalerie::removeDir($id);
			$scan = glob($data['gd_path'] . '/*');
            foreach($scan as $index => $path){
                @unlink($path);
                @unlink(str_replace('./galerie/', './galerie/thumbnails', $path));
            }
            @rmdir($data['gd_path']);
		}
		
		notice('Složky odebrány');
		include('files/Admin/Galerie/Display.inc');
		return;
	
	case 'addir':
	default:
		if(!post('name') && !post('parent')) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		if(post('parent') == 'none') {
			$parent = 0;
			$level = 2;
		} else {
			list($parent,$level) = explode('-', post('parent'));
			$level = ++$level;
		}
		$name = post('name');
		
		DBGalerie::addDir($name, $parent, $level);
		mkdir('galerie/' . mysql_insert_id());
		//FIXME: Galerie - addDir
		notice('Složka přidána');
		include('files/Admin/Galerie/Display.inc');
		return;
			
}
?>