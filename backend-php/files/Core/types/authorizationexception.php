<?php
class AuthorizationException extends ViewException
{
    public function getErrorFile() {
        return 'authorization';
    }
}