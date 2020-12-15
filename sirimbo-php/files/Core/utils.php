<?php
function formatTime($str, $forDisplay)
{
    if ($forDisplay) {
        return substr($str, 0, 5); //15:00:00
    } else {
        return $str . ':00';
    }
}

function formatDate($str)
{
    list($year, $month, $day) = explode('-', $str);
    return (int)$day . '. ' . (int)$month . '. ' . $year;
}

function formatTimestamp($str, $date_only = false)
{
    list($date, $time) = explode(' ', $str);
    if ($date_only) {
        return formatDate($date);
    }
    $date = formatDate($date);
    $time = formatTime($time, 1);
    return $date . ' ' . $time;
}

function formatRange($from, $to)
{
    $from = new \Date($from);
    $to = new \Date($to);
    return $from->getHumanDate() . ' - '  . $to->getHumanDate();
}

function rrmdir($dir)
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
            rrmdir($dir . DIRECTORY_SEPARATOR . $object);
        } else {
            unlink($dir . DIRECTORY_SEPARATOR . $object);
        }
    }
    unset($objects);
    rmdir($dir);
    return true;
}

function createThumbnail($file, $thumbFile)
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
    list($width, $height) = getimagesize($file);
    if ($width <= THUMBNAIL_MAX && $height <= THUMBNAIL_MAX) {
        $nWidth = $width;
        $nHeight = $height;
    } else {
        $scale = ($width > $height) ? (THUMBNAIL_MAX / $width) : (THUMBNAIL_MAX / $height);
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

function checkGetThumbnail($original)
{
    $thumbnail = str_replace(GALERIE, GALERIE_THUMBS, $original);
    if (is_file($thumbnail)) {
        return true;
    }
    if (!is_dir(dirname($thumbnail))) {
        mkdir(dirname($thumbnail), 0777, true);
    }
    return createThumbnail($original, $thumbnail);
}

function sanitizePathname(string $name)
{
    /** @var string */
    $x = preg_replace('([^\w\s\d\-_~,;:\[\]\(\]]|[\.]{2,})', '', $name);
    /** @var string */
    $x = preg_replace('/[^a-z0-9\._-]+/i', '-', $x);
    return strtolower($x);
}

function getCanonicalName($file)
{
    return trim(str_replace(GALERIE, '', $file), '/');
}
