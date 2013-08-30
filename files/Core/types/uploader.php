<?php
class Uploader {
	private $files = array();
	private $refusedFiles = array();
	private $savedFiles = array();
	private $allowedTypes = array();
	private $outputDir = '.';
	
	public function __construct() { }
	public function setOutputDir($dir) {
		if(file_exists($dir))
			$this->outputDir = $dir;
	}
	public function addAllowedType($type) {
		if(strpos($type, '.') === false)
			$type = '.' . $type;
		if(strpos($type, '*') !== false) {
			$type = str_replace('*', '', $type);
		}
		$this->allowedTypes[] = $type;
	}
	public function addTempFile($tempPath, $name, $size = 1) {
		if($size == 0)
			return;
		$this->files[] = array($tempPath, $name);
	}
	public function removeDisallowedFiles() {
		if(empty($this->allowedTypes))
			return;
		foreach($this->files as $key => $file) {
			$allowed = false;
			foreach($this->allowedTypes as $extension) {
				if(strripos($file[1], $extension) !== (strlen($file[1]) - strlen($extension)))
					continue;
				$allowed = true;
				break;
			}
			if(!$allowed) {
				$this->refusedFiles[] = $file;
				unset($this->files[$key]);
			}
		}
		return count($this->refusedFiles);
	}
	public function sanitizeFilenames() {
		foreach($this->files as &$file) {
			$strip = array('~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '=', '+', '[', '{', ']',
					'}', '\\', '|', ';', ':', '\"', '\'', '&#8216;', '&#8217;', '&#8220;', '&#8221;', '&#8211;', '&#8212;',
					'â€”', 'â€“', ',', '<', '>', '/', '?');
			$clean = trim(str_replace($strip, '', strip_tags($file[1])));
			$clean = preg_replace(array('/\s+/', '/\.\./'), array('-', '.'), $clean);
			
			$file[1] = $clean;
		}
	}
	public function getFiles() {
		return $this->files;
	}
	public function getRefusedFiles() {
		return $this->refusedFiles;
	}
	public function getSavedFiles() {
		return $this->savedFiles;
	}
	public function hasFiles() {
		return count($this->files) > 0;
	}
	public function hasRefusedFiles() {
		return count($this->refusedFiles) > 0;
	}
	public function save($sanitizeNames = true, $removeDisallowed = true) {
		if($sanitizeNames)
			$this->sanitizeFilenames();
		if($removeDisallowed)
			$this->removeDisallowedFiles();
		
		if(!file_exists($this->outputDir)) {
			$success = mkdir($this->outputDir, 0777, true);
			if(!$success) {
				Log::write('Could not create directory "' . $this->outputDir . '"');
				if((file_exists('./tmp') || mkdir('./tmp')))
					$this->outputDir = './tmp';
				else
					return false;
			}
		}
		
		foreach($this->files as $file) {
			$path = $this->outputDir . DIRECTORY_SEPARATOR . $file[1];
			move_uploaded_file($file[0], $path);
			chmod($path, 0666);
			$this->savedFiles[] = $path;
		}
		return true;
	}
}
?>