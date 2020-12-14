<?php
class NotFoundException extends ViewException
{
    public function getErrorFile()
    {
        return 'not_found';
    }
}
