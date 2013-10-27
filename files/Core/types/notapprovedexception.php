<?php
class NotApprovedException extends ViewException
{
    public function getErrorFile() {
        return 'not_approved';
    }
}