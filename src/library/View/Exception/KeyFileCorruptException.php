<?php
namespace TKOlomouc\View\Exception;

class KeyFileCorruptException extends ViewException
{
    public function getErrorFile() {
        return 'key_file_corrupt';
    }
}