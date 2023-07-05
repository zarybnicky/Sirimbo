<?php
interface Pagable
{
    public function getPage($offset, $count, $options = null);
    public function getCount($options = null);
}
