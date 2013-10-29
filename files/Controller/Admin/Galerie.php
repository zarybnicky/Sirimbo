<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Galerie extends Controller_Admin
{
    function __construct() {
        Permissions::checkError('galerie', P_OWNED);
    }
    function view($id = null) {
        if (empty($_POST)) {
            $this->render('files/Admin/Galerie/Display.inc');
            return;
        }
        switch(post('action')) {
            case 'save':
                $items = DBGalerie::getDirs();

                foreach ($items as $item) {
                    $id = $item['gd_id'];
                    if ((bool) post($id) !== (bool) $item['gd_hidden'])
                        DBGalerie::editDir(
                            $id, $item['gd_name'], $item['gd_id_rodic'],
                            $item['gd_level'], post($id) ? '1' : '0'
                        );
                }
                break;

            case 'edit':
                $galerie = post('galerie');
                if ($galerie[0])
                    $this->redirect('/admin/galerie/edit/' . $galerie[0]);
                break;
            case 'editdir':
                $galerie = post('galerie');
                if ($galerie[0])
                    $this->redirect('/admin/galerie/editdir/' . $galerie[0]);
                break;
            case 'remove':
                if (!is_array(post('galerie')))
                    break;
                $url = '/admin/galerie/remove?';
                foreach (post('galerie') as $id)
                    $url .= '&u[]=' . $id;
                $this->redirect($url);
                break;
            case 'addir':
            default:
                if (!post('name') && !post('parent')) {
                    $this->render('files/Admin/Galerie/Display.inc');
                    return;
                }
                if (post('parent') == 'none') {
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
                $this->redirect()->setMessage('Složka přidána');
                $this->render('files/Admin/Galerie/Display.inc');
                return;

        }
        $this->render('files/Admin/Galerie/Display.inc');
    }
    function remove($id = null) {
        if (empty($_POST) || post('action') !== 'confirm') {
            $this->render('files/Admin/Galerie/DisplayRemove.inc');
            return;
        }
        if (!is_array(post('galerie')))
            $this->redirect('/admin/galerie');
        foreach (post('galerie') as $id) {
            $data = DBGalerie::getSingleDir($id);
            DBGalerie::removeDir($id);
            $scan = glob($data['gd_path'] . '/*');
            if (is_array($scan))
                foreach ($scan as $index => $path) {
                    @unlink($path);
                    @unlink(str_replace(GALERIE, GALERIE . '/thumbnails', $path));
                }
            @rmdir($data['gd_path']);
        }
        $this->redirect('/admin/galerie', 'Složky odebrány');
    }
    function scan($id = null) {
        $db_dirs = DBGalerie::getDirsWithParentPath();
        $db_files = DBGalerie::getFotkyWithParentPath();
        $db_out_dirs = array();
        $db_out_files = array();

        foreach ($db_dirs as $dir) {
            $db_out_dirs[$dir['gd_path']] = $dir['gd_path_rodic'];
        }
        foreach ($db_files as $file) {
            $db_out_files[$file['gf_path']] = $file['gf_path_rodic'];
        }
        unset($db_dirs);
        unset($db_files);

        $fs_dirs = array();
        $fs_files = array();
        $fs_thumbnails = array();
        $fs_thumbnail_dirs = array();
        $this->_recursiveDirs(GALERIE, $fs_dirs, $fs_files);
        $this->_recursiveDirs(GALERIE_THUMBS, $fs_thumbnail_dirs, $fs_thumbnails);

        foreach ($fs_thumbnails as $file => $parent) {
            if (!isset($fs_files[str_replace(GALERIE_THUMBS, GALERIE, $file)]))
                unlink($file);
        }
        foreach ($fs_thumbnail_dirs as $dir => $parent) {
            if (!isset($fs_dirs[str_replace(GALERIE_THUMBS, GALERIE, $dir)]))
                $this->_rrmdir($dir);
        }
        unset($fs_thumbnails);
        unset($fs_thumbnail_dirs);

        foreach ($fs_dirs as $key => $parent) {
            $db_key = str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $key);
            if (isset($db_out_dirs[$db_key])
                && (GALERIE . DIRECTORY_SEPARATOR . $db_out_dirs[$db_key]
                    == $fs_dirs[$key])
            ) {
                unset($fs_dirs[$key]);
                unset($db_out_dirs[$db_key]);
            }
        }
        foreach ($fs_files as $key => $parent) {
            if (!is_file(str_replace(GALERIE, GALERIE_THUMBS, $key))
                && !$this->_createThumbnail($key)
            ) {
                unset($fs_files[$key]);
                continue;
            }
            $db_key = str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $key);
            if (isset($db_out_files[$db_key])
                && (GALERIE . DIRECTORY_SEPARATOR . $db_out_files[$db_key]
                    == $fs_files[$key])
            ) {
                unset($fs_files[$key]);
                unset($db_out_files[$db_key]);
            }
        }

        foreach ($db_out_dirs as $dir => $parent) {
            DBGalerie::removeDirByPath($dir);
        }
        foreach ($db_out_files as $file => $parent) {
            DBGalerie::removeFotoByPath($file);
        }

        asort($fs_dirs);
        foreach ($fs_dirs as $dir => $parent) {
            $db_dir = str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $dir);
            $parts = explode('/', $db_dir);
            $name = array_pop($parts);
            $level = count($parts) + 2;
            unset($parts);

            $tn_dir = str_replace(GALERIE, GALERIE_THUMBS, $dir);
            if (!is_dir($tn_dir))
                mkdir($tn_dir, 0777, true);

            DBGalerie::addDirByPath(
                $name,
                str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $parent),
                $level,
                $db_dir
            );
        }
        foreach ($fs_files as $file => $parent) {
            if (!file_exists(str_replace(GALERIE, GALERIE_THUMBS, $file)) && !$this->_createThumbnail($file))
                continue;

            $parts = explode(DIRECTORY_SEPARATOR, $file);
            $name = array_pop($parts);

            DBGalerie::addFotoByPath(
                str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $parent),
                str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $file),
                $name, User::getUserID()
            );
        }

        $this->redirect(
            '/admin/galerie',
            'Složek přidáno: ' . count($fs_dirs) . '<br/>' .
                'Souborů přidáno: ' . count($fs_files) . '<br/>' .
                '<br/>' .
                'Složek odebráno: ' . count($db_out_dirs) . '<br/>' .
                'Souborů odebráno: ' . count($db_out_files) . '<br/>'
        );
    }
    function upload($id = null) {
        if (empty($_POST)) {
            $this->render('files/Admin/Galerie/Upload.inc');
            return;
        }
        $dir = DBGalerie::getSingleDir(post('dir') ? post('dir') : 0);
        $files = $_FILES['file'];
        $names = post('name');

        $count = count($files['name']);
        for ($i = 0; $i < $count; $i++) {
            if ($files['error'][$i] > 0) {
                $this->redirect()->setMessage('Nepodařilo se nahrát soubor číslo ' . ($i + 1));
                continue;
            }
            if (!array_key_exists($files['type'][$i], Settings::$fotoTypes)) {
                $this->redirect()->setMessage(
                    'Soubor číslo ' . ($i + 1) .
                    ' není fotka podporovaného typu a byl přeskočen'
                );
                continue;
            }
            $fileUpload = $files['tmp_name'][$i];

            $fileName = $files['name'][$i];
            $fileName = str_replace(
                array('#', '$', '%', '&', '^', '*', '?'),
                array('No.', 'Dolar', 'Procento', 'And', ''),
                $fileName
            );
            $pieces = explode('.', $fileName);
            $fileExt = Settings::$fotoTypes[$files['type'][$i]];
            $pieces[count($pieces) - 1] = $fileExt;
            $fileName = implode('.', $pieces);
            $path = $dir['gd_path'] . DIRECTORY_SEPARATOR . $fileName;

            if (is_file(GALERIE . $path)) {
                $this->redirect()->setMessage(
                    'Soubor číslo ' . ($i + 1) .
                    ' nebo soubor se stejným názvem už existuje.'
                );
                continue;
            }

            if (!isset($names[$i]))
                $names[$i] = $fileName;

            DBGalerie::addFoto($dir['gd_id'], $path, $names[$i], User::getUserID());

            if (move_uploaded_file($fileUpload, GALERIE . DIRECTORY_SEPARATOR . $path)) {
                chmod(GALERIE . $path, 0666);
                $added = true;

                list($width, $height) = getimagesize(GALERIE . DIRECTORY_SEPARATOR . $path);
                if (!(($width > THUMBNAIL_MAX) || ($height > THUMBNAIL_MAX))) {
                    $nWidth = $width;
                    $nHeight = $height;
                } else {
                    $scale = ($width > $height) ? (THUMBNAIL_MAX / $width) : (THUMBNAIL_MAX / $height);
                    $nWidth = round($width * $scale);
                    $nHeight = round($height * $scale);
                }

                $fn_suffix = Settings::$gdFunctionSuffix[$files['type'][$i]];
                $fn_read = 'imageCreateFrom' . $fn_suffix;
                $fn_write = 'image' . $fn_suffix;
                if (!function_exists('imagebmp') || !function_exists('imagecreatefrombmp'))
                    $this->render('files/Core/bmp.php');

                if ($source = $fn_read(GALERIE . DIRECTORY_SEPARATOR . $path)) {
                    $thumbnail = imageCreateTruecolor($nWidth, $nHeight);

                    imageCopyResized(
                        $thumbnail, $source,
                        0, 0, 0, 0, $nWidth, $nHeight, $width, $height
                    );
                }
                $fn_write($thumbnail, GALERIE_THUMBS . DIRECTORY_SEPARATOR . $path);
                imageDestroy($thumbnail);
            } else {
                $this->redirect()->setMessage('Nepodařilo se nahrát soubor číslo ' . ($i + 1));

                $error = true;
            }
        }
        $n = new Novinky(User::getUserID());
        if (isset($error) && $error == true) {
            if (isset($added) && $added)
                $n->galerie()->edit($dir['gd_name']);
            $this->redirect('/admin/galerie', 'Bohužel, některé fotky se nepodařilo nahrát :o(');
        } else {
            if (isset($added) && $added)
                $n->galerie()->edit($dir['gd_name']);
            $this->redirect('/admin/galerie', 'Fotky přidány');
        }
    }
    function edit($id = null) {
        if (!is_numeric($id))
            $this->redirect('/admin/galerie', 'Složka s takovým ID neexistuje');

        if (empty($_POST)) {
            $this->render('files/Admin/Galerie/DisplayEdit.inc');
            return;
        }
        switch(post('action')) {
            case 'edit':
                $this->redirect()->setMessage('Not Implemented');//FIXME: Not Implemented - Foto edit (rename,...)
            case 'remove':
                $galerie = post('galerie');
                if (empty($galerie))
                    break;

                foreach ($galerie as $item_id) {
                    $data = DBGalerie::getSingleFoto($item_id);
                    DBGalerie::removeFoto($item_id);

                    unlink(GALERIE . DIRECTORY_SEPARATOR . $data['gf_path']);
                    unlink(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gf_path']);
                }
                $this->redirect()->setMessage('Fotky odebrány');
        }
        $this->render('files/Admin/Galerie/DisplayEdit.inc');
    }
    function editdir($id = null) {
        if (!is_numeric($id))
            $this->redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
        if (empty($_POST)) {
            $this->render('files/Admin/Galerie/DisplayDir.inc');
            return;
        }
        $f = new Form();
        $f->checkNotEmpty(post('name'), 'Složka musí mít jméno', 'name');
        $f->checkBool(post('parent') != 'none', 'Složka musí mít nadsložku', 'parent');
        if (!$f->isValid()) {
            $this->redirect()->setMessage($f->getMessages());
            $this->render('files/Admin/Galerie/DisplayDir.inc');
            return;
        }
        list($parent, $level) = explode('-', post('parent'));
        $level++;

        DBGalerie::editDir($id, post('name'), $parent, $level, post('hidden') ? '1' : '0');
        $this->redirect('/admin/galerie', 'Složka úspěšně upravena');
    }

    private function _recursiveDirs($dir_name, &$out_dirs, &$out_files) {
        $file_list = scandir($dir_name);

        foreach ($file_list as $key => $file) {
            if (in_array($file, array('.','..','thumbnails', '.gitignore'))) {
                unset($file_list[$key]);
                continue;
            }
            $file_list[$key] = $dir_name . DIRECTORY_SEPARATOR . $file;

            if (is_dir($file_list[$key])) {
                $out_dirs[$file_list[$key]] = ($dir_name == GALERIE ? GALERIE . DIRECTORY_SEPARATOR . '0' : $dir_name);
                $this->_recursiveDirs($file_list[$key], $out_dirs, $out_files);
            } elseif(is_file($file_list[$key])) {
                $out_files[$file_list[$key]] = $dir_name;
            }
        }
    }
    private function _rrmdir($dir) {
        if (!is_dir($dir))
            return false;

        $objects = scandir($dir);
        foreach ($objects as $object) {
            if (!in_array($object, array('.', '..'))) {
                if (is_dir($dir . DIRECTORY_SEPARATOR . $object))
                    $this->_rrmdir($dir . DIRECTORY_SEPARATOR . $object);
                else
                    unlink($dir . DIRECTORY_SEPARATOR . $object);
            }
        }
        unset($objects);
        rmdir($dir);
    }
    private function _createThumbnail($file) {
        $filetype = image_type_to_mime_type(exif_imagetype($file));
        if (!$filetype || !array_key_exists($filetype, Settings::$fotoTypes)) {
            unset($fs_files[$file]);
            unlink($file);
            return false;
        }

        list($width, $height) = getimagesize($file);
        if (!(($width > THUMBNAIL_MAX) || ($height > THUMBNAIL_MAX))) {
            $nWidth = $width;
            $nHeight = $height;
        } else {
            $scale = ($width > $height) ? (THUMBNAIL_MAX / $width) : (THUMBNAIL_MAX / $height);
            $nWidth = round($width * $scale);
            $nHeight = round($height * $scale);
        }

        $fn_suffix = Settings::$gdFunctionSuffix[$filetype];
        $fn_read = 'imageCreateFrom' . $fn_suffix;
        $fn_write = 'image' . $fn_suffix;
        if (!function_exists('imagebmp') || !function_exists('imagecreatefrombmp'))
            $this->render('files/Core/bmp.php');

        if ($source = $fn_read($file)) {
            $thumbnail = imageCreateTruecolor($nWidth, $nHeight);

            imageCopyResized($thumbnail, $source,
            0, 0, 0, 0, $nWidth, $nHeight, $width, $height);
        }
        $tn_path = str_replace(GALERIE, GALERIE_THUMBS, $file);
        if (!is_dir(dirname($tn_path)))
            mkdir(dirname($tn_path), 0777, true);

        $fn_write($thumbnail, $tn_path);
        imageDestroy($thumbnail);
        return true;
    }
}
?>