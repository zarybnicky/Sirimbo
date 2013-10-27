<?php
class KeyFileCorruptException extends ViewException
{
    public function getErrorFile() {
        return 'key_file_corrupt';
    }
}