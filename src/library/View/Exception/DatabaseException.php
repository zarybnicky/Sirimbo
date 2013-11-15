<?php
namespace TKOlomouc\View\Exception;

class DatabaseException extends ViewException
{
    public function getErrorFile() {
        return 'database';
    }
}