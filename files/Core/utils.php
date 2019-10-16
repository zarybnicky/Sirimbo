<?php
function formatTime($str, $forDisplay) {
    if ($forDisplay) {
        return substr($str, 0, 5); //15:00:00
    } else {
        return $str . ':00';
    }
}

function formatDate($str) {
    list($year, $month, $day) = explode('-', $str);
    return (int)$day . '. ' . (int)$month . '. ' . $year;
}

function formatTimestamp($str, $date_only = false) {
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
    $from = new Date($from);
    $to = new Date($to);
    return $from->getDate(Date::FORMAT_SIMPLIFIED)
        . ' - '
        . $to->getDate(Date::FORMAT_SIMPLIFIED);
}

function rrmdir($dir)
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
            rrmdir($dir . DIRECTORY_SEPARATOR . $object);
        } else {
            unlink($dir . DIRECTORY_SEPARATOR . $object);
        }
    }
    unset($objects);
    rmdir($dir);
}

function createThumbnail($file, $thumbFile)
{
    $filetype = image_type_to_mime_type(exif_imagetype($file));
    if (!$filetype || !array_key_exists($filetype, Settings::$imageType)) {
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

    $fn_suffix = Settings::$imageSuffix[$filetype];
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

function sanitizePathname($name)
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

function getCanonicalName($file)
{
    return trim(str_replace(GALERIE, '', $file), '/');
}

if (!function_exists('getallheaders')) {
    function getallheaders() {
        $headers = [];
        foreach ($_SERVER as $name => $value) {
            if (substr($name, 0, 5) == 'HTTP_') {
                $headers[str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', substr($name, 5)))))] = $value;
            }
        }
        return $headers;
    }
}
