<?php
class BanException extends ViewException
{
    public function getErrorFile() {
        return 'ban';
    }
}