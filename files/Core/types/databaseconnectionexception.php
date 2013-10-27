<?php
class DatabaseConenctionException extends ViewException {
    public function getErrorFile() {
        return 'database_connection';
    }
}