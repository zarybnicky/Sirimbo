<?php
namespace TKOlomouc\View\Exception;

class DatabaseConnectionException extends ViewException
{
    public function getErrorFile() {
        return 'database_connection';
    }
}