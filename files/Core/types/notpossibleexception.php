<?php
class NotPossibleException extends ViewException
{
    public function getErrorFile() {
        return 'not_possible';
    }
}