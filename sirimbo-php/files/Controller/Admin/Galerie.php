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
        new \RedirectHelper('/admin/galerie');
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
                rrmdir($dir);
            }
        }
        unset($fsThumbnails);
        unset($fsThumbnailDirs);

        //Check for new image directories
        foreach ($fsDirs as $key => $parent) {
            $db_key = getCanonicalName($key);
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
            if (!checkGetThumbnail($key)) {
                unlink($key);
                unset($fsFiles[$key]);
                continue;
            }
            $db_key = getCanonicalName($key);
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
            $db_dir = getCanonicalName($dir);
            $parts = explode(DIRECTORY_SEPARATOR, $db_dir);
            $level = count($parts) + 1;
            $name = array_pop($parts);
            if (!$name) {
                $name = 'Hlavní';
            }
            unset($parts);

            \DBGalerie::addDirByPath(
                $name,
                getCanonicalName($parent),
                $level,
                $db_dir
            );
        }

        //Inserting new files
        foreach ($fsFiles as $file => $parent) {
            $parts = explode(DIRECTORY_SEPARATOR, $file);
            $name = array_pop($parts);

            \DBGalerie::addFotoByPath(
                getCanonicalName($parent),
                getCanonicalName($file),
                $name,
                \Session::getUserID()
            );
        }
        new \MessageHelper(
            'info',
            'Složek přidáno: ' . count($fsDirs) . '<br>' .
            'Souborů přidáno: ' . count($fsFiles) . '<br>' .
            '<br>' .
            'Složek odebráno: ' . count($dbDirs) . '<br>' .
            'Souborů odebráno: ' . count($dbFiles)
        );
        new \RedirectHelper('/admin/galerie');
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
}
