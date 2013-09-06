<?php
class CSVParser implements Iterator {
	private $fileObject = null;
	private $delimiter = ';';
	private $enclosure = '"';
	private $escape = '\\';
	private $associative = false;
	private $headers = array();
	private $encoding = 'UTF-8';
	private $recode = false;
	
	public function __construct(SplFileObject $file, $readHeaders = true) {
		if($file->isReadable())
			$this->fileObject = $file;
		else
			throw new InvalidArgumentException("Given file $file is not readable");
		
		$text = $this->fileObject->current();
		$this->encoding = mb_detect_encoding($text, 'UTF-8,ISO-8859-2,ISO-8859-1,cp1250');
		
		if($this->encoding === false)
			$this->encoding = 'UTF-8';
		if($this->encoding != 'UTF-8')
			$this->recode = true;
		
		$this->fileObject->setFlags(SplFileObject::DROP_NEW_LINE);
		$this->fileObject->rewind();
		
		if($readHeaders) {
			$this->headers = $this->current();
			$this->next();
		}
		return $this;
	}
	public function delimiter($char = null) {
		if($char === null)
			return $this->delimiter;
		$this->delimiter = $char;
		return $this;
	}
	public function enclosure($char = null) {
		if($char === null)
			return $this->enclosure;
		$this->enclosure = $char;
		return $this;
	}
	public function escape($char = null) {
		if($char === null)
			return $this->escape;
		$this->escape = $char;
		return $this;
	}
	public function associative($bool = null) {
		if($bool === null)
			return $this->associative;
		$this->associative = $bool;
		return $this;
	}
	public function headers() {
		return $this->headers;
	}
	public function getFileObject() {
		return $this->fileObject;
	}
	public function current() {
		if($this->headers && !$this->key()) {
			$this->fileObject->current();
			$this->next();
		}
		if($this->recode)
			$parsed = mb_convert_encoding($this->fileObject->current(), 'UTF-8', $this->encoding);
		else
			$parsed = $this->fileObject->current();
		
		$parsed = str_getcsv($parsed, $this->delimiter, $this->enclosure, $this->escape);
		
		if(count($parsed) < 1 || !$parsed[0])
			return null;
		if(!$this->associative || !$this->headers)
			return $parsed;
		return array_combine($this->headers, $parsed);
	}
	public function rewind() {
		$this->fileObject->rewind();
	}
	public function next() {
		$this->fileObject->next();
	}
	public function key() {
		return $this->fileObject->key();
	}
	public function valid() {
		return $this->fileObject->valid();
	}
}