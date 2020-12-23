<?php
namespace Olymp\Controller\Admin;

class Galerie
{
    public static function list()
    {
        \Permissions::checkError('galerie', P_OWNED);
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::galleryDir($item['gd_id']),
                'name' => str_repeat('&nbsp;->', $item['gd_level'] - 1) . ' ' . $item['gd_name'],
                'hidden' => new \CheckboxHelper($item['gd_id'], '1', $item['gd_hidden'])
            ],
            \DBGalerie::getDirs(true, true),
        );
        new \RenderHelper('files/View/Admin/Galerie/Overview.inc', [
            'header' => 'Správa fotogalerie',
            'data' => $data
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('galerie', P_OWNED);
        if ($_POST['action'] == 'save') {
            static::_processSave();
        }
        if ($_POST['action'] == 'scan') {
            static::_scan();
        }
        \Redirect::to('/admin/galerie');
    }

    private static function _scan()
    {
        $dbInDirs = \DBGalerie::getDirsWithParentPath();
        $dbInFiles = \DBGalerie::getFotkyWithParentPath();
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
        static::_recursiveDirs(GALERIE, $fsDirs, $fsFiles);
        static::_recursiveDirs(GALERIE_THUMBS, $fsThumbnailDirs, $fsThumbnails);

        //Redundant thumbnails
        foreach ($fsThumbnails as $file => $parent) {
            if (!isset($fsFiles[str_replace(GALERIE_THUMBS, GALERIE, $file)])) {
                unlink($file);
            }
        }
        //Reduntant thumbnail directories
        foreach ($fsThumbnailDirs as $dir => $parent) {
            if (!isset($fsDirs[str_replace(GALERIE_THUMBS, GALERIE, $dir)])) {
                self::rrmdir($dir);
            }
        }
        unset($fsThumbnails);
        unset($fsThumbnailDirs);

        //Check for new image directories
        foreach ($fsDirs as $key => $parent) {
            $db_key = self::getCanonicalName($key);
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
            if (!self::checkGetThumbnail($key)) {
                unlink($key);
                unset($fsFiles[$key]);
                continue;
            }
            $db_key = self::getCanonicalName($key);
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
            \DBGalerie::removeDirByPath($dir);
        }
        foreach ($dbFiles as $file => $parent) {
            \DBGalerie::removeFotoByPath($file);
        }

        //Inserting new directories
        asort($fsDirs);
        foreach ($fsDirs as $dir => $parent) {
            $tn_dir = str_replace(GALERIE, GALERIE_THUMBS, $dir);
            if (!is_dir($tn_dir)) {
                mkdir($tn_dir, 0777, true);
            }
            $db_dir = self::getCanonicalName($dir);
            $parts = explode(DIRECTORY_SEPARATOR, $db_dir);
            $level = count($parts) + 1;
            $name = array_pop($parts);
            if (!$name) {
                $name = 'Hlavní';
            }
            unset($parts);

            \DBGalerie::addDirByPath(
                $name,
                self::getCanonicalName($parent),
                $level,
                $db_dir
            );
        }

        //Inserting new files
        foreach ($fsFiles as $file => $parent) {
            $parts = explode(DIRECTORY_SEPARATOR, $file);
            $name = array_pop($parts);

            \DBGalerie::addFotoByPath(
                self::getCanonicalName($parent),
                self::getCanonicalName($file),
                $name,
                \Session::getUser()->getId()
            );
        }
        \Message::info(
            'Složek přidáno: ' . count($fsDirs) . '<br>' .
            'Souborů přidáno: ' . count($fsFiles) . '<br>' .
            '<br>' .
            'Složek odebráno: ' . count($dbDirs) . '<br>' .
            'Souborů odebráno: ' . count($dbFiles)
        );
        \Redirect::to('/admin/galerie');
    }

    private static function _recursiveDirs($dir_name, &$out_dirs, &$out_files)
    {
        $file_list = scandir($dir_name);
        if (!$file_list) {
            return;
        }
        foreach ($file_list as $key => $file) {
            if (in_array($file, ['.', '..', 'thumbnails', '.gitignore'])) {
                unset($file_list[$key]);
                continue;
            }
            $file_list[$key] = $dir_name . DIRECTORY_SEPARATOR . $file;
            if (is_dir($file_list[$key])) {
                $out_dirs[$file_list[$key]] = $dir_name;
                static::_recursiveDirs($file_list[$key], $out_dirs, $out_files);
            } elseif (is_file($file_list[$key])) {
                $out_files[$file_list[$key]] = $dir_name;
            }
        }
    }

    private static function _processSave()
    {
        $items = \DBGalerie::getDirs();
        foreach ($items as $item) {
            if ((bool) $_POST[$item['gd_id']] === (bool) $item['gd_hidden']) {
                continue;
            }
            \DBGalerie::editDir(
                $item['gd_id'],
                $item['gd_name'],
                $item['gd_id_rodic'],
                $item['gd_level'],
                $_POST[$item['gd_id']] ? '1' : '0',
                $item['gd_path']
            );
        }
    }

    public static function getCanonicalName(string $file): string
    {
        return trim(str_replace(GALERIE, '', $file), '/');
    }

    public static function sanitizePathname(string $name): string
    {
        $x = preg_replace('([^\w\s\d\-_~,;:\[\]\(\]]|[\.]{2,})', '', $name);
        $x = preg_replace('/[^a-z0-9\._-]+/i', '-', $x);
        return strtolower($x);
    }

    public static function rrmdir($dir)
    {
        if (!is_dir($dir)) {
            return false;
        }
        $objects = scandir($dir);
        if ($objects === false) {
            return false;
        }
        foreach ($objects as $object) {
            if (in_array($object, ['.', '..'])) {
                continue;
            }
            if (is_dir($dir . DIRECTORY_SEPARATOR . $object)) {
                self::rrmdir($dir . DIRECTORY_SEPARATOR . $object);
            } else {
                unlink($dir . DIRECTORY_SEPARATOR . $object);
            }
        }
        unset($objects);
        rmdir($dir);
        return true;
    }

    public static function createThumbnail($file, $thumbFile)
    {
        $type = exif_imagetype($file);
        if (!$type) {
            unlink($file);
            return false;
        }
        $filetype = image_type_to_mime_type($type);
        if (!$filetype || !array_key_exists($filetype, \Settings::$imageType)) {
            unlink($file);
            return false;
        }
        $thumbnailMax = 150;
        list($width, $height) = getimagesize($file);
        if ($width <= $thumbnailMax && $height <= $thumbnailMax) {
            $nWidth = $width;
            $nHeight = $height;
        } else {
            $scale = ($width > $height) ? ($thumbnailMax / $width) : ($thumbnailMax / $height);
            $nWidth = round($width * $scale);
            $nHeight = round($height * $scale);
        }

        /** @var callable */
        $fn_read = 'imageCreateFrom' . \Settings::$imageSuffix[$filetype];
        if (!($source = $fn_read($file))) {
            return false;
        }
        $thumbnail = imagecreatetruecolor($nWidth, $nHeight);
        if (!$thumbnail) {
            return false;
        }
        imagecopyresized(
            $thumbnail, $source,
            0, 0, 0, 0,
            $nWidth, $nHeight, $width, $height
        );
        /** @var callable */
        $fn_write = 'image' . \Settings::$imageSuffix[$filetype];
        $fn_write($thumbnail, $thumbFile);
        imagedestroy($thumbnail);
        return true;
    }

    public static function checkGetThumbnail($original)
    {
        $thumbnail = str_replace(GALERIE, GALERIE_THUMBS, $original);
        if (is_file($thumbnail)) {
            return true;
        }
        if (!is_dir(dirname($thumbnail))) {
            mkdir(dirname($thumbnail), 0777, true);
        }
        return self::createThumbnail($original, $thumbnail);
    }
}
