<?php
namespace Olymp\Controller\Admin;

class Galerie
{
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
