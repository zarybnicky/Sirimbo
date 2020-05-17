<?php
class NotFoundRightException extends ViewException
{
    public function getErrorFile() {
        return 'not_found_right';
    }
}