<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;
use TKOlomouc\Utility\UploadHandler;
use TKOlomouc\Utility\Log;

class Upload extends Partial
{
    private $name         = '';
    private $validFiles   = array();
    private $invalidFiles = array();
    private $emptyFiles   = array();
    private $hasFiles     = false;

    public function __construct($twig, $name = null)
    {
        parent::__construct($twig);

        if ($name !== null) {
            $this->name($name);
        }
        return $this;
    }

    public function name($name)
    {
        $this->name = $name;

        return $this;
    }

    public function loadFromPost()
    {
        if (!isset($files[$this->name]) || empty($files[$this->name])) {
            return array();
        }
        if (!is_array($files[$this->name])) {
            $input = array($files[$this->name]);
        } else {
            foreach ($files[$this->name] as $key => $data) {
                foreach ($data as $i => $value) {
                    $input[$i][$key] = $value;
                }
            }
        }
        if (!empty($input)) {
            $this->hasFiles = true;
        }
        foreach ($input as $data) {
            $this->processFilesItem($data);
        }
        return $this;
    }

    private function processFilesItem($data) {
        $error = $data['error'];
        switch($error) {
            case UPLOAD_ERR_OK:
                if ($data['size'] > 0) {
                    $this->validFiles[] = $data;
                } else {
                    $this->emptyFiles[] = $data;
                }
                return true;
            case UPLOAD_ERR_NO_FILE:
                $this->emptyFiles[] = $data;
                return true;
            case UPLOAD_ERR_INI_SIZE:
            case UPLOAD_ERR_FORM_SIZE:
                $logError = false;
                $errorMessage = 'Soubor "' . $data['name'] . '" byl příliš velký, '
                    . 'největši povolená velikost souboru je '
                    . ini_get('upload_maxfilesize') . 'B';
                break;
            case UPLOAD_ERR_PARTIAL:
                $logError = false;
                $errorMessage = 'Nahrávání souboru bylo přerušeno, zkuste to prosím znovu.';
                break;
            case UPLOAD_ERR_CANT_WRITE:
            case UPLOAD_ERR_EXTENSION:
            case UPLOAD_ERR_NO_TMP_DIR:
                $logError = true;
                $errorMessage = 'Došlo k chybě při ukládání souboru (kód: '
                    . $error . '), kontaktujte prosím administrátora.';
                break;
        }
        if (isset($logError) && $logError) {
            Log::write($errorMessage);
        }
        $this->invalidFiles[] = array_merge(
            $data,
            array('error_message' => $errorMessage)
        );
        return false;
    }

    public function getFilledUploadHandler()
    {
        $uploader = new UploadHandler();
        foreach ($this->validFiles as $file) {
            $uploader->addTempFile($file['tmpname'], $file['name'], $file['size']);
        }
        return $uploader;
    }

    public function hasFiles()
    {
        return $this->hasFiles;
    }

    public function hasValidFiles()
    {
        return !empty($this->validFiles);
    }

    public function hasEmptyFiles()
    {
        return !empty($this->emptyFiles);
    }

    public function hasInvalidFiles()
    {
        return !empty($this->invalidFiles);
    }

    public function getValidFiles()
    {
        return $this->validFiles;
    }

    public function getEmptyFiles()
    {
        return $this->emptyFiles;
    }

    public function getInvalidFiles()
    {
        return $this->invalidFiles;
    }

    public function getErrorMessages()
    {
        $messages = array();
        foreach ($this->invalidFiles as $data) {
            $messages[] = $data['error_message'];
        }
        return $messages;
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render()
    {
        if ($this->name === null) {
            return '';
        }
        return "<input name='{$this->name}[]' multiple='multiple' type='file'/>";
    }
}
