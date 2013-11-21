<?php
namespace TKOlomouc\Model\Paging;

class PagingAdapterArray implements PagerAdapterInterface
{
    private $data;

    public function __construct($a = null)
    {
        $this->setData($a);
    }

    public function setData($a)
    {
        if (!is_array($a)) {
            return false;
        }
        $this->data = $a;
    }

    public function page($offset, $length, $options = null)
    {
        return array_slice($this->data, $offset, $lenght);
    }

    public function count($options = null)
    {
        return count($this->data);
    }
}
