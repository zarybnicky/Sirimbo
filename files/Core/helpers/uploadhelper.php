<?php
class UploadHelper {
    private $name;
    private $files;
    private $invalidFiles;
    private $emptyFiles;
    private $hasFiles;
    
    public function upload($name = null) {
        $this->_defaultValues();
        if($name !== null)
            $this->name($name);
        
        return $this;
    }
    private function _defaultValues() {
        $this->name = null;
        $this->files = array();
        $this->invalidFiles = array();
        $this->emptyFiles = array();
        $this->hasFiles = false;
    }
    public function name($name) {
        $this->name = $name;
        return $this;
    }
    public function loadFromPost() {
        if(!isset($_FILES[$this->name]) || empty($_FILES[$this->name]))
            return array();
        
        if(!is_array($_FILES[$this->name])) {
            $input = array($_FILES[$this->name]);
        } else {
            foreach($_FILES[$this->name] as $key => $data) {
                foreach($data as $i => $value) {
                    $input[$i][$key] = $value;
                }
            }
        }
        if(!empty($input))
            $this->hasFiles = true;
        
        foreach($input as $data) {
            $this->processFilesItem($data);
        }
        return $this;
    }
    private function processFilesItem($data) {
        $error = $data['error'];
        switch($error) {
            case UPLOAD_ERR_OK:
                if($data['size'] > 0)
                    $this->files[] = $data;
                else
                    $this->emptyFiles[] = $data;
                return true;
            case UPLOAD_ERR_NO_FILE:
                $this->emptyFiles[] = $data;
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
        if(isset($logError) && $logError)
            Log::write($errorMessage);
        $this->invalidFiles[] = array_merge($data, array(
                'error_message' => $errorMessage
        ));
        return false;
    }
    public function getFilledUploader($loadFromPost = false) {
        if($loadFromPost)
            $this->loadFromPost();
        
        $uploader = new Uploader();
        foreach($this->files as $file) {
            $uploader->addTempFile($file['tmp_name'], $file['name'], $file['size']);
        }
        return $uploader;
    }
    public function hasFiles() {
        return $this->hasFiles;
    }
    public function hasValidFiles() {
        return !empty($this->files);
    }
    public function hasEmptyFiles() {
        return !empty($this->emptyFiles);
    }
    public function hasInvalidFiles() {
        return !empty($this->invalidFiles);
    }
    public function getValidFiles() {
        return $this->files;
    }
    public function getEmptyFiles() {
        return $this->emptyFiles;
    }
    public function getInvalidFiles() {
        return $this->invalidFiles;
    }
    public function getErrorMessages() {
        if(empty($this->invalidFiles))
            return array();
        $messages = array();
        foreach($this->invalidFiles as $data) {
            $messages[] = $data['error_message'];
        }
        return $messages;
    }
    public function __toString() {
        return $this->render();
    }
    public function render() {
        if($this->name === null)
            return '';
        return '<input name="' . $this->name . '[]" multiple="multiple" type="file"/>';
    }
}