<?php
interface Pagable
{
    public function getPage($offset, $lenght, $options = null);
    public function getCount($options = null);
}
