<?php
class AutoLoader
{
    static private $_classNames = array();
    /**
     * Store the filename (sans extension) & full path of all ".php" files found
    */
    public static function registerDirectory($dirName)
    {
        $di = new DirectoryIterator($dirName);
        foreach ($di as $file) {
            if ($file->isDir() && !$file->isLink() && !$file->isDot()) {
                self::registerDirectory($file->getPathname());
            } elseif (substr($file->getFilename(), -4) === '.php') {
                $className = substr($file->getFilename(), 0, -4);
                AutoLoader::registerClass($className, $file->getPathname());
            }
        }
    }
    public static function registerClass($className, $fileName)
    {
        AutoLoader::$_classNames[$className] = $fileName;
    }
    public static function loadClass($className)
    {
        if (isset(AutoLoader::$_classNames[$className])) {
            include_once AutoLoader::$_classNames[$className];
        }
    }
}
spl_autoload_register(array('AutoLoader', 'loadClass'));
AutoLoader::registerDirectory(__DIR__ . '/../files');