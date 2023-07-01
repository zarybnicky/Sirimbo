<?php
class DatabaseConnectionException extends ViewException
{
    public function getErrorFile() {
        return 'database_connection';
    }
}