<?php
class UploadHelper
{
    private $_name;
    private $_files;
    private $_invalidFiles;
    private $_emptyFiles;
    private $_hasFiles;

    public function upload($name = null) {
        $this->_defaultValues();
        if ($name !== null)
            $this->_name($name);

        return $this;
    }
    private function _defaultValues() {
        $this->_name = null;
        $this->_files = array();
        $this->_invalidFiles = array();
        $this->_emptyFiles = array();
        $this->_hasFiles = false;
    }
    public function name($name) {
        $this->_name = $name;
        return $this;
    }
    public function loadFromPost() {
        if (!isset($_FILES[$this->_name]) || empty($_FILES[$this->_name]))
            return array();

        if (!is_array($_FILES[$this->_name])) {
            $input = array($_FILES[$this->_name]);
        } else {
            foreach ($_FILES[$this->_name] as $key => $data) {
                foreach ($data as $i => $value) {
                    $input[$i][$key] = $value;
                }
            }
        }
        if (!empty($input))
            $this->_hasFiles = true;

        foreach ($input as $data) {
            $this->_processFilesItem($data);
        }
        return $this;
    }
    private function _processFilesItem($data) {
        $error = $data['error'];
        switch($error) {
            case UPLOAD_ERR_OK:
                if ($data['size'] > 0)
                    $this->_files[] = $data;
                else
                    $this->_emptyFiles[] = $data;
                return true;
            case UPLOAD_ERR_NO_FILE:
                $this->_emptyFiles[] = $data;
                return true;
            case UPLOAD_ERR_INI_SIZE:
            case UPLOAD_ERR_FORM_SIZE:
                $logError = false;
                $errorMessage = 'Soubor "' . $data['name'] . '" byl příliš velký, ' .
                        'největši povolená velikost souboru je ' . ini_get('upload_max_filesize') . 'B';
                break;
            case UPLOAD_ERR_PARTIAL:
                $logError = false;
                $errorMessage = 'Nahrávání souboru bylo přerušeno, zkuste to prosím znovu.';
                break;
            case UPLOAD_ERR_CANT_WRITE:
            case UPLOAD_ERR_EXTENSION:
            case UPLOAD_ERR_NO_TMP_DIR:
                $logError = true;
                $errorMessage = 'Došlo k chybě při ukládání souboru (kód: ' . $error . '), kontaktujte prosím administrátora.';
                break;
        }
        if (isset($logError) && $logError)
            Log::write($errorMessage);
        $this->_invalidFiles[] = array_merge($data, array(
                'error_message' => $errorMessage
        ));
        return false;
    }
    public function getFilledUploader($loadFromPost = false) {
        if ($loadFromPost)
            $this->loadFromPost();

        $uploader = new Uploader();
        foreach ($this->_files as $file) {
            $uploader->addTempFile($file['tmp_name'], $file['name'], $file['size']);
        }
        return $uploader;
    }
    public function hasFiles() {
        return $this->_hasFiles;
    }
    public function hasValidFiles() {
        return !empty($this->_files);
    }
    public function hasEmptyFiles() {
        return !empty($this->_emptyFiles);
    }
    public function hasInvalidFiles() {
        return !empty($this->_invalidFiles);
    }
    public function getValidFiles() {
        return $this->_files;
    }
    public function getEmptyFiles() {
        return $this->_emptyFiles;
    }
    public function getInvalidFiles() {
        return $this->_invalidFiles;
    }
    public function getErrorMessages() {
        if (empty($this->_invalidFiles))
            return array();
        $messages = array();
        foreach ($this->_invalidFiles as $data) {
            $messages[] = $data['error_message'];
        }
        return $messages;
    }
    public function __toString() {
        return $this->render();
    }
    public function render() {
        if ($this->_name === null)
            return '';
        return '<input name="' . $this->_name . '[]" multiple="multiple" type="file"/>';
    }
}