<?php
namespace TKOlomouc\View\Exception;

class NotFoundRightException extends ViewException
{
    public function getErrorFile() {
        return 'not_found_right';
    }
}