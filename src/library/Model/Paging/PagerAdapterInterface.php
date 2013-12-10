<?php
namespace TKOlomouc\Model\Paging;

interface PagerAdapterInterface
{
    public function page($offset, $length, $options = null);
    public function count($options = null);
}
