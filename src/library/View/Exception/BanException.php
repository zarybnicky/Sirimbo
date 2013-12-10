<?php
namespace TKOlomouc\View\Exception;

class BanException extends ViewException
{
    public function getErrorFile()
    {
        return 'ban';
    }
}
