<?php
interface Pagable
{
    public static function getPage($offset, $lenght, $options = null);
    public static function getCount($options = null);
}