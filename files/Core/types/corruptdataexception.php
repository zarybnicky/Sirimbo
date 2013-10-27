<?php
class CorruptDataException extends ViewException {
    public function getErrorFile() {
        return 'corrupt_data';
    }
}