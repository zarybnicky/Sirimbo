<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Galerie extends Controller_Admin
{
    protected $imageType = [
        'image/pjpeg' => 'jpg',
        'image/jpeg' => 'jpg',
        'image/gif' => 'gif',
        'image/bmp' => 'bmp',
        'image/x-png' => 'png'
    ];

    protected $imageSuffix = [
        'image/pjpeg' => 'JPEG',
        'image/jpeg' => 'JPEG',
        'image/gif' => 'GIF',
        'image/bmp' => 'BMP',
        'image/x-png' => 'PNG'
    ];

    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function view($request)
    {
        switch ($request->post('action')) {
            case 'save':
                $this->_processSave($request);
                break;
            case 'scan':
                $this->_scan();
                break;
        }

        $data = DBGalerie::getDirs(true, true);
        $data = array_map(
            function ($item) {
                return [
                    'buttons' => (
                        $this->editLink('/admin/galerie/directory/edit/' . $item['gd_id']) . '&nbsp;' .
                        $this->duplicateLink('/admin/galerie/directory/' . $item['gd_id']) . '&nbsp;' .
                        $this->removeLink('/admin/galerie/directory/remove/' . $item['gd_id'])
                    ),
                    'name' => str_repeat('&nbsp;->', $item['gd_level'] - 1) . ' ' . $item['gd_name'],
                    'hidden' => (string) $this->checkbox($item['gd_id'], '1') ->set($item['gd_hidden'])
                ];
            },
            $data
        );

        $this->render(
            'files/View/Admin/Galerie/Overview.inc',
            ['data' => $data]
        );
    }

    private function _scan()
    {
        $dbInDirs = DBGalerie::getDirsWithParentPath();
        $dbInFiles = DBGalerie::getFotkyWithParentPath();
        $dbDirs = [];
        $dbFiles = [];

        foreach ($dbInDirs as $dir) {
            $dbDirs[$dir['gd_path']] = $dir['gd_path_rodic'];
        }
        foreach ($dbInFiles as $file) {
            $dbFiles[$file['gf_path']] = $file['gf_path_rodic'];
        }
        unset($dbInDirs);
        unset($dbInFiles);

        $fsDirs = [];
        $fsThumbnailDirs = [];
        $fsFiles = [];
        $fsThumbnails = [];
        $this->_recursiveDirs(GALERIE, $fsDirs, $fsFiles);
        $this->_recursiveDirs(GALERIE_THUMBS, $fsThumbnailDirs, $fsThumbnails);

        //Redundant thumbnails
        foreach ($fsThumbnails as $file => $parent) {
            if (!isset($fsFiles[str_replace(GALERIE_THUMBS, GALERIE, $file)])) {
                unlink($file);
            }
        }
        //Reduntant thumbnail directories
        foreach ($fsThumbnailDirs as $dir => $parent) {
            if (!isset($fsDirs[str_replace(GALERIE_THUMBS, GALERIE, $dir)])) {
                $this->_rrmdir($dir);
            }
        }
        unset($fsThumbnails);
        unset($fsThumbnailDirs);

        //Check for new image directories
        foreach ($fsDirs as $key => $parent) {
            $db_key = $this->_getCanonicalName($key);
            if (array_key_exists($db_key, $dbDirs)
                && (($dbDirs[$db_key] ? GALERIE . DIRECTORY_SEPARATOR . $dbDirs[$db_key] : GALERIE)
                == $fsDirs[$key])
            ) {
                unset($fsDirs[$key]);
                unset($dbDirs[$db_key]);
            }
        }

        //Check for new images
        foreach ($fsFiles as $key => $parent) {
            //If can't get a thumbnail, delete the file
            if (!$this->_checkGetThumbnail($key)) {
                unlink($key);
                unset($fsFiles[$key]);
                continue;
            }
            $db_key = $this->_getCanonicalName($key);
            if (array_key_exists($db_key, $dbFiles)
                && (GALERIE . DIRECTORY_SEPARATOR . $dbFiles[$db_key]
                == $fsFiles[$key])
            ) {
                unset($fsFiles[$key]);
                unset($dbFiles[$db_key]);
            }
        }

        //Remove deleted files (in DB but not in filesystem)
        foreach ($dbDirs as $dir => $parent) {
            if (!$dir) {
                unset($dbDirs[$dir]);
                continue;
            }
            DBGalerie::removeDirByPath($dir);
        }
        foreach ($dbFiles as $file => $parent) {
            DBGalerie::removeFotoByPath($file);
        }

        //Inserting new directories
        asort($fsDirs);
        foreach ($fsDirs as $dir => $parent) {
            $tn_dir = str_replace(GALERIE, GALERIE_THUMBS, $dir);
            if (!is_dir($tn_dir)) {
                mkdir($tn_dir, 0777, true);
            }
            $db_dir = $this->_getCanonicalName($dir);
            $parts = explode(DIRECTORY_SEPARATOR, $db_dir);
            $level = count($parts) + 1;
            $name = array_pop($parts);
            if (!$name) {
                $name = 'Hlavní';
            }
            unset($parts);

            DBGalerie::addDirByPath(
                $name,
                $this->_getCanonicalName($parent),
                $level,
                $db_dir
            );
        }

        //Inserting new files
        foreach ($fsFiles as $file => $parent) {
            $parts = explode(DIRECTORY_SEPARATOR, $file);
            $name = array_pop($parts);

            DBGalerie::addFotoByPath(
                $this->_getCanonicalName($parent),
                $this->_getCanonicalName($file),
                $name, User::getUserID()
            );
        }
        $this->redirect()
            ->setMessage('Složek přidáno: ' . count($fsDirs))
            ->setMessage('Souborů přidáno: '   . count($fsFiles))
            ->setMessage('')
            ->setMessage('Složek odebráno: '   . count($dbDirs))
            ->setMessage('Souborů odebráno: '  . count($dbFiles))
            ->sendRedirect('/admin/galerie');
    }

    protected function _sanitizePathname($name)
    {
        return strtolower(
            preg_replace(
                '/[^a-z0-9\._-]+/i', '-',
                preg_replace(
                    '([^\w\s\d\-_~,;:\[\]\(\]]|[\.]{2,})', '',
                    $name
                )
            )
        );
    }

    protected function _rrmdir($dir)
    {
        if (!is_dir($dir)) {
            return false;
        }
        $objects = scandir($dir);
        foreach ($objects as $object) {
            if (in_array($object, ['.', '..'])) {
                continue;
            }
            if (is_dir($dir . DIRECTORY_SEPARATOR . $object)) {
                $this->_rrmdir($dir . DIRECTORY_SEPARATOR . $object);
            } else {
                unlink($dir . DIRECTORY_SEPARATOR . $object);
            }
        }
        unset($objects);
        rmdir($dir);
    }

    protected function _getCanonicalName($file)
    {
        return trim(str_replace(GALERIE, '', $file), '/');
    }

    protected function _checkGetThumbnail($original)
    {
        $thumbnail = str_replace(GALERIE, GALERIE_THUMBS, $original);
        if (is_file($thumbnail)) {
            return true;
        }
        if (!is_dir(dirname($thumbnail))) {
            mkdir(dirname($thumbnail), 0777, true);
        }
        return $this->_createThumbnail($original, $thumbnail);
    }

    private function _recursiveDirs($dir_name, &$out_dirs, &$out_files)
    {
        $file_list = scandir($dir_name);
        foreach ($file_list as $key => $file) {
            if (in_array($file, ['.', '..', 'thumbnails', '.gitignore'])) {
                unset($file_list[$key]);
                continue;
            }
            $file_list[$key] = $dir_name . DIRECTORY_SEPARATOR . $file;
            if (is_dir($file_list[$key])) {
                $out_dirs[$file_list[$key]] = $dir_name;
                $this->_recursiveDirs($file_list[$key], $out_dirs, $out_files);
            } elseif (is_file($file_list[$key])) {
                $out_files[$file_list[$key]] = $dir_name;
            }
        }
    }

    private function _createThumbnail($file, $thumbFile)
    {
        $filetype = image_type_to_mime_type(exif_imagetype($file));
        if (!$filetype || !array_key_exists($filetype, $this->imageType)) {
            unlink($file);
            return false;
        }
        list($width, $height) = getimagesize($file);
        if ($width <= THUMBNAIL_MAX && $height <= THUMBNAIL_MAX) {
            $nWidth = $width;
            $nHeight = $height;
        } else {
            $scale = ($width > $height) ? (THUMBNAIL_MAX / $width) : (THUMBNAIL_MAX / $height);
            $nWidth = round($width * $scale);
            $nHeight = round($height * $scale);
        }

        $fn_suffix = $this->imageSuffix[$filetype];
        if ($fn_suffix == 'BMP') {
            include 'files/Core/bmp.php';
        }
        $fn_read = 'imageCreateFrom' . $fn_suffix;
        $fn_write = 'image' . $fn_suffix;

        if (!($source = $fn_read($file))) {
            return false;
        }
        $thumbnail = imageCreateTruecolor($nWidth, $nHeight);
        imageCopyResized(
            $thumbnail, $source,
            0, 0, 0, 0,
            $nWidth, $nHeight, $width, $height
        );
        $fn_write($thumbnail, $thumbFile);
        imageDestroy($thumbnail);
        return true;
    }

    private function _processSave($request)
    {
        $items = DBGalerie::getDirs();
        foreach ($items as $item) {
            if ((bool) $request->post($item['gd_id']) === (bool) $item['gd_hidden']) {
                continue;
            }
            DBGalerie::editDir(
                $item['gd_id'],
                $item['gd_name'],
                $item['gd_id_rodic'],
                $item['gd_level'],
                $request->post($item['gd_id']) ? '1' : '0',
                $item['gd_path']
            );
        }
    }
}
