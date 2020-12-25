<?php
class UploadHelper
{
    private ?string $name;
    private $files = [];
    private $invalidFiles = [];

    public function __construct(string $name)
    {
        $this->name = $name;
    }

    public function loadFromPost()
    {
        $files = $_FILES[$this->name];
        if (!$files) {
            $this->files = [];
            return $this;
        }
        $input = [];
        if (!is_array($files)) {
            $input = [$files];
        } else {
            foreach ($files as $key => $data) {
                foreach ($data as $i => $value) {
                    $input[$i][$key] = $value;
                }
            }
        }
        foreach ($input as $data) {
            $this->processFilesItem($data);
        }
        return $this;
    }

    private function processFilesItem($data)
    {
        switch ($data['error']) {
            case UPLOAD_ERR_OK:
            case UPLOAD_ERR_NO_FILE:
                $this->files[] = $data;
                break;

            case UPLOAD_ERR_INI_SIZE:
            case UPLOAD_ERR_FORM_SIZE:
                $errorMessage = "Soubor '{$data['name']}' byl příliš velký, "
                    . 'maximální velikost souboru je ' . ini_get('upload_max_filesize') . 'B';
                $this->invalidFiles[] = array_merge($data, ['error_message' => $errorMessage]);
                break;

            case UPLOAD_ERR_PARTIAL:
                $errorMessage = 'Nahrávání souboru bylo přerušeno, zkuste to prosím znovu.';
                $this->invalidFiles[] = array_merge($data, ['error_message' => $errorMessage]);
                break;

            case UPLOAD_ERR_CANT_WRITE:
            case UPLOAD_ERR_EXTENSION:
            case UPLOAD_ERR_NO_TMP_DIR:
                $errorMessage = "Došlo k chybě při ukládání souboru (kód: {$data['error']}), kontaktujte prosím administrátora.";
                syslog(LOG_ERR, "Failed to save file: " . var_export($data, true) . "\n");
                $this->invalidFiles[] = array_merge($data, ['error_message' => $errorMessage]);
                break;
        }
    }

    public function getFilledUploader()
    {
        $uploader = new Uploader();
        foreach ($this->files as $file) {
            $uploader->addTempFile($file['tmp_name'], $file['name'], $file['size']);
        }
        return $uploader;
    }

    public function getErrorMessages()
    {
        $messages = [];
        foreach ($this->invalidFiles as $data) {
            $messages[] = $data['error_message'];
        }
        return $messages;
    }

    public function __toString()
    {
        if ($this->name === null) {
            return '';
        }
        return "<input type=\"file\" multiple name=\"{$this->name}[]\" class=\"form-control-file\">";
    }
}
