<?php
class Controller_Admin_Galerie implements Controller_Interface {
	function __construct() {
		Permissions::checkError('galerie', P_OWNED);
	}
	function view($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Galerie/Display.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				$galerie = post('galerie');
				if($galerie[0])
					View::redirect('/admin/galerie/edit/' . $galerie[0]);
				break;
			case 'editdir':
				$galerie = post('galerie');
				if($galerie[0])
					View::redirect('/admin/galerie/editdir/' . $galerie[0]);
				break;
			case 'remove':
				if(!is_array(post('galerie')))
					break;
				$url = '/admin/galerie/remove?';
				foreach(post('galerie') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
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
				
				$parent_data = DBGalerie::getSingleDir($parent);
				mkdir(str_replace('/', DIRECTORY_SEPARATOR, $parent_data['gd_path']) . DIRECTORY_SEPARATOR . $name, 0777);
				
				DBGalerie::addDir($name, $parent, $level);
				//FIXME: Galerie - test addDir
				notice('Složka přidána');
				include('files/Admin/Galerie/Display.inc');
				return;
					
		}
	}
	function remove($id = null) {
		if(empty($_POST) || post('action') !== 'confirm') {
			include('files/Admin/Galerie/DisplayRemove.inc');
			return;
		}
		if(!is_array(post('galerie')))
			View::redirect('/admin/galerie');
		foreach(post('galerie') as $id) {
			$data = DBGalerie::getSingleDir($id);
			DBGalerie::removeDir($id);
			$scan = glob($data['gd_path'] . '/*');
			if(is_array($scan))
	            foreach($scan as $index => $path){
	                @unlink($path);
	                @unlink(str_replace('./galerie/', './galerie/thumbnails', $path));
	            }
            @rmdir($data['gd_path']);
		}
		View::redirect('/admin/galerie', 'Složky odebrány');
	}
	function scan($id = null) {
		$db_dirs = DBGalerie::getDirsWithParentPath();
		$db_files = DBGalerie::getFotkyWithParentPath();
		$db_out_dirs = array();
		$db_out_files = array();
		
		foreach($db_dirs as $dir) {
			$db_out_dirs[$dir['gd_path']] = $dir['gd_path_rodic'];
		}
		foreach($db_files as $file) {
			$db_out_files[$file['gf_path']] = $file['gf_path_rodic'];
		}
		unset($db_dirs);
		unset($db_files);
		
		$fs_dirs = array();
		$fs_files = array();
		$fs_thumbnails = array();
		$fs_thumbnail_dirs = array();
		$this->recursiveDirs("./galerie", $fs_dirs, $fs_files);
		$this->recursiveDirs("./galerie/thumbnails", $fs_thumbnail_dirs, $fs_thumbnails);
		
		foreach($fs_thumbnail_dirs as $dir => $parent) {
			if(!isset($fs_dirs[str_replace('./galerie/thumbnails/', './galerie/', $dir)]))
				$this->rrmdir($dir);
		}
		unset($fs_thumbnails);
		unset($fs_thumbnail_dirs);
		
		foreach($fs_dirs as $key => $parent) {
			if(!isset($db_out_dirs[$key]) || ($db_out_dirs[$key] != $fs_dirs[$key]))
				continue;
			unset($fs_dirs[$key]);
			unset($db_out_dirs[$key]);
		}
		foreach($fs_files as $key => $parent) {
			if(!is_file(str_replace('./galerie', './galerie/thumbnails', $key)))
				$this->createThumbnail($key, $parent);
			
			if(!isset($db_out_files[$key]) || ($db_out_files[$key] != $fs_files[$key]))
				continue;
			unset($fs_files[$key]);
			unset($db_out_files[$key]);
		}
		
		foreach($db_out_dirs as $dir => $parent) {
			DBGalerie::removeDirByPath($dir);
		}
		foreach($db_out_files as $file => $parent) {
			DBGalerie::removeFotoByPath($file);
		}
		
		asort($fs_dirs);
		foreach($fs_dirs as $dir => $parent) {
			$parts = explode('/', $dir);
			$name = array_pop($parts);
			$level = count($parts);
			unset($parts);
			
			$tn_dir = str_replace("./galerie/", './galerie/thumbnails/', $dir);
			if(!is_dir($tn_dir))
				mkdir($tn_dir, 0777, true);
			
			DBGalerie::addDirByPath($name, $parent, $level, $dir);
		}
		
		foreach($fs_files as $file => $parent) {
			if(!$this->createThumbnail($file, $parent))
				continue;
			
			$parts = explode('/', $file);
			$name = array_pop($parts);
			
			DBGalerie::addFotoByPath($parent, $file, $name, User::getUserID());
		}
		
		View::redirect('/admin/galerie', 'Složek přidáno: ' . count($fs_dirs) . '<br/>' .
			'Souborů přidáno: ' . count($fs_files) . '<br/>' .
			'<br/>' .
			'Složek odebráno: ' . count($db_out_dirs) . '<br/>' .
			'Souborů odebráno: ' . count($db_out_files) . '<br/>');
	}
	function upload($id = null) {
		if(empty($_POST)) {
			include('files/Admin/Galerie/Upload.inc');
			return;
		}
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
	function edit($id = null) {
		if(!is_numeric($id))
			View::redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
		
		if(empty($_POST)) {
			include('files/Admin/Galerie/DisplayEdit.inc');
			return;
		}
		switch(post('action')) {
			case 'edit':
				notice('Not Implemented');//FIXME: Not Implemented - Foto edit (rename,...)
			case 'remove':
				$galerie = post('galerie');
				if(empty($galerie))
					break;
				
				foreach($galerie as $item_id) {
					$data = DBGalerie::getSingleFoto($item_id);
					DBGalerie::removeFoto($item_id);
					
					unlink($data['gf_path']);
					unlink(str_replace('./galerie', './galerie/thumbnails', $data['gd_path']));
				}
				notice('Fotky odebrány');
		}
		include('files/Admin/Galerie/DisplayEdit.inc');
	}
	function editdir($id = null) {
		if(!is_numeric($id))
			View::redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
		if(empty($_POST)) {
			include('files/Admin/Galerie/DisplayDir.inc');
			return;
		}
		$f = new Form();
		$f->checkNotEmpty(post('name'), 'Složka musí mít jméno', 'name');
		$f->checkBool(post('parent') != 'none', 'Složka musí mít nadsložku', 'parent');
		if(!$f->isValid()) {
			notice($f->getMessages());
			include('files/Admin/Galerie/DisplayDir.inc');
			return;
		}
		list($parent, $level) = explode('-', post('parent'));
		$level++;
		
		DBGalerie::editDir($id, post('name'), $parent, $level, post('hidden') ? '1' : '0');
		View::redirect('/admin/galerie', 'Složka úspěšně upravena');
	}
	
	
	private function recursiveDirs($dir_name, &$out_dirs, &$out_files) {
		$file_list = scandir($dir_name);
		
		foreach($file_list as $key => $file) {
			if(in_array($file, array('.','..','thumbnails'))) {
				unset($file_list[$key]);
				continue;
			}
			$file_list[$key] = $dir_name . '/' . $file;
							
			if(is_dir($file_list[$key])) {
				$out_dirs[$file_list[$key]] = ($dir_name == './galerie' ? './galerie/0' : $dir_name);
				$this->recursiveDirs($file_list[$key], $out_dirs, $out_files);
			} else if(!strstr($file_list[$key], '/tn_')) {
				$out_files[$file_list[$key]] = ($dir_name == './galerie' ? './galerie/0' : $dir_name);
			}
		}
	}
	private function rrmdir($dir) {
		if(!is_dir($dir))
			return false;
		
		$objects = scandir($dir);
		foreach($objects as $object) {
			if(!in_array($object, array('.', '..'))) {
				if(is_dir($dir . '/' . $object))
					$this->rrmdir($dir . '/' . $object);
				else
					unlink($dir . '/' . $object);
			}
		}
		unset($objects);
		rmdir($dir);
	}
	private function createThumbnail($file, $parent) {
		$filetype = image_type_to_mime_type(exif_imagetype($file));
		if(!$filetype || !array_key_exists($filetype, Settings::$foto_types)) {
			unset($fs_files[$file]);
			unlink($file);
			return false;
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
		$tn_dir = str_replace('./galerie', './galerie/thumbnails', $parent);
		$tn_path = str_replace('./galerie', './galerie/thumbnails', $file);
		if(!is_dir($tn_dir))
			mkdir($tn_dir, 0777, true);
		
		$fn_write($thumbnail, $tn_path);
		imageDestroy($thumbnail);
		return true;
	}
}
?>