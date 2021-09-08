<?php
class CSVParser implements Iterator
{
    private $_file = null;
    private $_delimiter = ',';
    private $_enclosure = '"';
    private $_escape = '\\';
    private $_associative = false;
    private $_headers = [];
    private $_encoding = 'UTF-8';
    private $_recode = false;

    public function __construct(SplFileObject $file, $readHeaders = true)
    {
        if (!$file->isReadable()) {
            throw new ViewException("Given file $file is not readable");
        }
        $this->_file = $file;

        /** @var string */
        $text = $file->current();
        $this->_encoding = mb_detect_encoding($text, 'UTF-16LE, UTF-8, ISO-8859-2, ISO-8859-1', true);

        if ($this->_encoding === false) {
            $this->_recode = 'cp1250';
        } elseif ($this->_encoding != 'UTF-8') {
            $this->_recode = true;
        }

        $this->_file->setFlags(SplFileObject::DROP_NEW_LINE);
        $this->_file->rewind();

        if ($readHeaders) {
            $this->_headers = $this->current();
            $this->next();
        }
    }
    public function delimiter($char = null) {
        if ($char === null)
            return $this->_delimiter;
        $this->_delimiter = $char;
        return $this;
    }
    public function enclosure($char = null) {
        if ($char === null)
            return $this->_enclosure;
        $this->_enclosure = $char;
        return $this;
    }
    public function escape($char = null) {
        if ($char === null)
            return $this->_escape;
        $this->_escape = $char;
        return $this;
    }
    public function associative($bool = null) {
        if ($bool === null)
            return $this->_associative;
        $this->_associative = $bool;
        return $this;
    }
    public function headers() {
        return $this->_headers;
    }
    public function getFileObject() {
        return $this->_file;
    }
    public function current() {
        if ($this->_headers && !$this->key()) {
            $this->_file->current();
            $this->next();
        }
        $parsed = $parsed = $this->_file->current();
        if ($this->_recode === 'cp1250') {
            $parsed = iconv('windows-1250', 'UTF-8', $parsed);
        } elseif ($this->_recode) {
            $parsed = mb_convert_encoding($parsed, 'UTF-8', $this->_encoding);
        }

        $parsed = str_getcsv($parsed, $this->_delimiter, $this->_enclosure, $this->_escape);

        if (count($parsed) < 1 || !$parsed[0])
            return null;
        if (!$this->_associative || !$this->_headers)
            return $parsed;
        return array_combine($this->_headers, $parsed);
    }
    public function rewind() {
        $this->_file->rewind();
    }
    public function next() {
        $this->_file->next();
    }
    public function key() {
        return $this->_file->key();
    }
    public function valid() {
        return $this->_file->valid();
    }
}
