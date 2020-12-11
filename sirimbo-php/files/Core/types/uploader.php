<?php
class Uploader
{
    private $_files = [];
    private $_refusedFiles = [];
    private $_savedFiles = [];
    private $_allowedTypes = [];
    private $_outputDir = '.';

    public function __construct()
    {
    }

    public function setOutputDir($dir)
    {
        $this->_outputDir = $dir;
    }

    public function addAllowedType($type)
    {
        if (strpos($type, '.') === false) {
            $type = '.' . $type;
        }
        if (strpos($type, '*') !== false) {
            $type = str_replace('*', '', $type);
        }
        $this->_allowedTypes[] = $type;
    }

    public function addTempFile($tempPath, $name, $size = 1)
    {
        if ($size == 0) {
            return;
        }
        $this->_files[] = [$tempPath, $name];
    }

    public function removeDisallowedFiles()
    {
        if (empty($this->_allowedTypes)) {
            return;
        }
        foreach ($this->_files as $key => $file) {
            $allowed = false;
            foreach ($this->_allowedTypes as $extension) {
                if (strripos($file[1], $extension) !== (strlen($file[1]) - strlen($extension))) {
                    continue;
                }
                $allowed = true;
                break;
            }
            if (!$allowed) {
                $this->_refusedFiles[] = $file;
                unset($this->_files[$key]);
            }
        }
        return count($this->_refusedFiles);
    }

    public function sanitizeFilenames()
    {
        foreach ($this->_files as &$file) {
            $strip = [
                '~', '`', '!', '@', '#', '$', '%', '^', '&', '*',
                '(', ')', '_', '=', '+', '[', '{', ']', '}', '\\',
                '|', ';', ':', '\"', '\'', '&#8216;', '&#8217;',
                '&#8220;', '&#8221;', '&#8211;', '&#8212;',
                'â€”', 'â€“', ',', '<', '>', '/', '?'
            ];
            $clean = trim(str_replace($strip, '', strip_tags($file[1])));
            $clean = preg_replace(['/\s+/', '/\.\./'], ['-', '.'], $clean);

            $file[1] = $clean;
        }
    }

    public function getFiles()
    {
        return $this->_files;
    }

    public function getRefusedFiles()
    {
        return $this->_refusedFiles;
    }

    public function getSavedFiles()
    {
        return $this->_savedFiles;
    }

    public function hasFiles()
    {
        return !empty($this->_files);
    }

    public function hasRefusedFiles()
    {
        return !empty($this->_refusedFiles);
    }

    public function save($sanitizeNames = true, $removeDisallowed = true)
    {
        if ($sanitizeNames) {
            $this->sanitizeFilenames();
        }
        if ($removeDisallowed) {
            $this->removeDisallowedFiles();
        }
        if (!file_exists($this->_outputDir)) {
            $success = mkdir($this->_outputDir, 0777, true);
            if (!$success) {
                fwrite(STDERR, "Failed to create directory '{$this->_outputDir}', trying to write to ./tmp");
                if (!(file_exists('./tmp') && !mkdir('./tmp'))) {
                    return false;
                }
                $this->_outputDir = './tmp';
            }
        }
        foreach ($this->_files as $file) {
            $path = $this->_outputDir . DIRECTORY_SEPARATOR . $file[1];
            move_uploaded_file($file[0], $path);
            chmod($path, 0666);
            $this->_savedFiles[] = $path;
        }
        return true;
    }
}
