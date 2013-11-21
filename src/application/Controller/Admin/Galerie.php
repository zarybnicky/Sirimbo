<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\BMP;
use TKOlomouc\Model\DBGalerie;
use TKOlomouc\Settings;

class Galerie extends Admin
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function view($id = null)
    {
        switch(post('action')) {
            case 'galerie/save':
                $this->processSave();
                break;
            case 'galerie/scan':
                $this->scan();
                break;
            case 'directory':
            case 'directory/edit':
                $galerie = post('galerie');
                if (isset($galerie[0])) {
                    $this->redirect(
                        '/admin/galerie/' . post('action') . '/' . $galerie[0]
                    );
                }
                break;
            case 'directory/remove':
                if (is_array(post('galerie'))) {
                    $this->redirect(
                        '/admin/galerie/directory/remove?'
                            . http_build_query(array('u' => post('galerie')))
                    );
                }
                break;
        }
        $this->displayOverview();
    }

    private function scan($id = null)
    {
        $dbInDirs = DBGalerie::getDirsWithParentPath();
        $dbInFiles = DBGalerie::getFotkyWithParentPath();
        $dbDirs = array();
        $dbFiles = array();

        foreach ($dbInDirs as $dir) {
            $dbDirs[$dir['gd_path']] = $dir['gd_path_rodic'];
        }
        foreach ($dbInFiles as $file) {
            $dbFiles[$file['gf_path']] = $file['gf_path_rodic'];
        }
        unset($dbInDirs);
        unset($dbInFiles);

        $fsDirs = array(
        	GALERIE . DIRECTORY_SEPARATOR => GALERIE . DIRECTORY_SEPARATOR
        );
        $fsThumbnailDirs = array(
        	GALERIE_THUMBS . DIRECTORY_SEPARATOR => GALERIE_THUMBS . DIRECTORY_SEPARATOR
        );
        $fsFiles = array();
        $fsThumbnails = array();
        $this->recursiveDirs(GALERIE, $fsDirs, $fsFiles);
        $this->recursiveDirs(GALERIE_THUMBS, $fsThumbnailDirs, $fsThumbnails);

        //Redundant thumbnails
        foreach ($fsThumbnails as $file => $parent) {
            if (!isset($fsFiles[str_replace(GALERIE_THUMBS, GALERIE, $file)])) {
                unlink($file);
            }
        }
        //Reduntant thumbnail directories
        foreach ($fsThumbnailDirs as $dir => $parent) {
            if (!isset($fsDirs[str_replace(GALERIE_THUMBS, GALERIE, $dir)])) {
                $this->rrmdir($dir);
            }
        }
        unset($fsThumbnails);
        unset($fsThumbnailDirs);

        //Check for new image directories
        foreach ($fsDirs as $key => $parent) {
            $db_key = $this->getCanonicalName($key);
            if (isset($dbDirs[$db_key])
                && (GALERIE . DIRECTORY_SEPARATOR . $dbDirs[$db_key]
                == $fsDirs[$key])
            ) {
                unset($fsDirs[$key]);
                unset($dbDirs[$db_key]);
            }
        }

        //Check for new images
        foreach ($fsFiles as $key => $parent) {
            //If can't get a thumbnail, delete the file
            if (!$this->checkGetThumbnail($key)) {
                unlink($key);
                unset($fsFiles[$key]);
                continue;
            }
            $db_key = $this->getCanonicalName($key);
            if (isset($dbFiles[$db_key])
                && (GALERIE . DIRECTORY_SEPARATOR . $dbFiles[$db_key]
                == $fsFiles[$key])
            ) {
                unset($fsFiles[$key]);
                unset($dbFiles[$db_key]);
            }
        }

        //Remove deleted files (in DB but not in filesystem)
        foreach ($dbDirs as $dir => $parent) {
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
            $db_dir = $this->getCanonicalName($dir);
            $parts = explode(DIRECTORY_SEPARATOR, $db_dir);
            $level = count($parts) + 1;
            $name = array_pop($parts);
            if (!$name) {
                $name = 'Hlavní';
            }
            unset($parts);

            DBGalerie::addDirByPath(
                $name,
                $this->getCanonicalName($parent),
                $level,
                $db_dir
            );
        }

        //Inserting new files
        foreach ($fsFiles as $file => $parent) {
            $parts = explode(DIRECTORY_SEPARATOR, $file);
            $name = array_pop($parts);

            DBGalerie::addFotoByPath(
                $this->getCanonicalName($parent),
                $this->getCanonicalName($file),
                $name, User::getUserID()
            );
        }
        $this->redirect(
            '/admin/galerie',
            'Složek přidáno: '      . count($fsDirs) . '<br/>'
            . 'Souborů přidáno: '   . count($fsFiles) . '<br/>'
            . '<br/>'
            . 'Složek odebráno: '   . count($dbDirs) . '<br/>'
            . 'Souborů odebráno: '  . count($dbFiles) . '<br/>'
        );
    }

    protected function sanitizePathname($name)
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

    protected function rrmdir($dir)
    {
        if (!is_dir($dir)) {
            return false;
        }
        $objects = scandir($dir);
        foreach ($objects as $object) {
            if (in_array($object, array('.', '..'))) {
                continue;
            }
            if (is_dir($dir . DIRECTORY_SEPARATOR . $object)) {
                $this->rrmdir($dir . DIRECTORY_SEPARATOR . $object);
            } else {
                unlink($dir . DIRECTORY_SEPARATOR . $object);
            }
        }
        unset($objects);
        rmdir($dir);
    }

    protected function getCanonicalName($file)
    {
        return trim(str_replace(GALERIE, '', $file), '/');
    }

    protected function checkGetThumbnail($file)
    {
        if (is_file(str_replace(GALERIE, GALERIE_THUMBS, $file))) {
            return true;
        }
        $thumbFile = str_replace(GALERIE, GALERIE_THUMBS, $file);
        if (!is_dir(dirname($thumbFile))) {
            mkdir(dirname($thumbFile), 0777, true);
        }
        return $this->createThumbnail($file, $thumbFile);
    }

    private function recursiveDirs($dir_name, &$out_dirs, &$out_files)
    {
        $file_list = scandir($dir_name);
        foreach ($file_list as $key => $file) {
            if (in_array($file, array('.','..','thumbnails', '.gitignore'))) {
                unset($file_list[$key]);
                continue;
            }
            $file_list[$key] = $dir_name . DIRECTORY_SEPARATOR . $file;
            if (is_dir($file_list[$key])) {
                $out_dirs[$file_list[$key]] = $dir_name;
                $this->recursiveDirs($file_list[$key], $out_dirs, $out_files);
            } elseif (is_file($file_list[$key])) {
                $out_files[$file_list[$key]] = $dir_name;
            }
        }
    }

    private function createThumbnail($file, $thumbFile)
    {
        $filetype = image_type_to_mime_type(exif_imagetype($file));
        if (!$filetype || !array_key_exists($filetype, Settings::$fotoTypes)) {
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

        $fn_suffix = Settings::$gdFunctionSuffix[$filetype];
        if ($fn_suffix == 'BMP') {
            BMP;
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

    private function processSave()
    {
        $items = DBGalerie::getDirs();
        foreach ($items as $item) {
            if ((bool) post($item['gd_id']) === (bool) $item['gd_hidden']) {
                continue;
            }
            DBGalerie::editDir(
                $item['gd_id'], $item['gd_name'], $item['gd_id_rodic'],
                $item['gd_level'], post($item['gd_id']) ? '1' : '0',
                $item['gd_path']
            );
        }
    }

    private function displayOverview()
    {
        $data = DBGalerie::getDirs(true, true);
        foreach ($data as &$item) {
            $new_data = array(
            	'checkBox'  => getCheckbox('galerie[]', $item['gd_id']),
                'name'      => str_repeat('&nbsp;->', $item['gd_level'] - 1)
                    . ' ' . $item['gd_name'],
                'hidden'    => getCheckbox($item['gd_id'], '1', $item['gd_hidden'])
            );
            $item = $new_data;
        }
        $this->render(
            'src/application/View/Admin/Galerie/Overview.inc',
            array('data' => $data)
        );
    }
}
