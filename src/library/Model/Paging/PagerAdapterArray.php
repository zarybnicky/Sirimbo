<?php
namespace TKOlomouc\Model\Paging;

class PagingAdapterArray implements PagerAdapterInterface
{
    private $data;

    function __construct($a = null)
    {
        $this->setData($a);
    }

    function setData($a)
    {
        if (!isdata($a))
            return false;
        $this->data = $a;
    }

    function page($offset, $length, $options = null)
    {
        return array_slice($this->data, $offset, $lenght);
    }

    function count($options = null)
    {
        return count($this->data);
    }
}