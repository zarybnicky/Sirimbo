<?php
namespace TKOlomouc\View\Exception;

class NotApprovedException extends ViewException
{
    public function getErrorFile()
    {
        return 'not_approved';
    }
}
