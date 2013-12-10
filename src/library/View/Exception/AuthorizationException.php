<?php
namespace TKOlomouc\View\Exception;

class AuthorizationException extends ViewException
{
    public function getErrorFile()
    {
        return 'authorization';
    }
}
