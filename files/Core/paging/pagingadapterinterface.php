<?php
interface PagingAdapterInterface
{
    public function page($offset, $length, $options = null);
    public function count($options = null);
}