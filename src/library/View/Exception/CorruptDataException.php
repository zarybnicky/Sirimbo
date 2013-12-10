<?php
namespace TKOlomouc\View\Exception;

class CorruptDataException extends ViewException
{
    public function getErrorFile()
    {
        return 'corrupt_data';
    }
}
