<?php
class Uploader
{
    private $files = [];
    private $errors = [];
    private $refusedFiles = [];
    private $savedFiles = [];
    private $allowedTypes = [];
    private $outputDir = '.';

    public function getUploadErrors()
    {
        return $this->errors;
    }

    public function getSavedFiles()
    {
        return $this->savedFiles;
    }

    public function save()
    {
        if ($this->allowedTypes) {
            foreach ($this->files as $key => $file) {
                foreach ($this->allowedTypes as $extension) {
                    if (strripos($file[1], $extension) === (strlen($file[1]) - strlen($extension))) {
                        continue 2;
                    }
                }
                $this->refusedFiles[] = $file;
                unset($this->files[$key]);
            }
        }

        foreach ($this->files as $file) {
            $strip = [
                '~', '`', '!', '@', '#', '$', '%', '^', '&', '*',
                '(', ')', '_', '=', '+', '[', '{', ']', '}', '\\',
                '|', ';', ':', '\"', '\'', '&#8216;', '&#8217;',
                '&#8220;', '&#8221;', '&#8211;', '&#8212;',
                'â€”', 'â€“', ',', '<', '>', '/', '?'
            ];
            $clean = trim(str_replace($strip, '', strip_tags($file[1])));
            $file[1] = preg_replace(['/\s+/', '/\.\./'], ['-', '.'], $clean);

            $path = $this->outputDir . DIRECTORY_SEPARATOR . $file[1];
            move_uploaded_file($file[0], $path);
            chmod($path, 0666);
            $this->savedFiles[] = $path;
        }
        return [$this->savedFiles, $this->refusedFiles];
    }

    public function __construct($files, $outputDir, $allowedTypes = [])
    {
        $this->outputDir = $outputDir;
        $this->allowedTypes = array_map(
            fn($x) => strpos($x, '.') === false ? ".$x" : $x,
            $allowedTypes,
        );

        if (!is_array($files)) {
            $input = [$files];
        } else {
            $input = [];
            foreach ($files as $key => $data) {
                foreach ($data as $i => $value) {
                    $input[$i][$key] = $value;
                }
            }
        }
        foreach ($input as $data) {
            switch ($data['error']) {
                case UPLOAD_ERR_OK:
                    if ($data['size']) {
                        $this->files[] = [$data['tmp_name'], $data['name']];
                    }
                    break;

                case UPLOAD_ERR_INI_SIZE:
                case UPLOAD_ERR_FORM_SIZE:
                    $this->errors[] = "Soubor '{$data['name']}' byl příliš velký, "
                        . 'maximální velikost souboru je ' . ini_get('upload_max_filesize') . 'B';
                    break;

                case UPLOAD_ERR_NO_FILE:
                case UPLOAD_ERR_PARTIAL:
                    $this->errors[] = "Nahrávání souboru '{$data['name']}' bylo přerušeno, zkuste to prosím znovu.";
                    break;

                case UPLOAD_ERR_CANT_WRITE:
                case UPLOAD_ERR_EXTENSION:
                case UPLOAD_ERR_NO_TMP_DIR:
                    syslog(LOG_ERR, "Failed to save file: " . print_r($data, true) . "\n");
                    $this->errors[] = "Došlo k chybě při ukládání souboru (kód: {$data['error']}), kontaktujte prosím administrátora.";
                    break;
            }
        }
    }
}
