<?php
namespace TKOlomouc\Model\Paging;

use TKOlomouc\Model\Database\Pagable;
use TKOlomouc\View\Exception\ViewException;

class PagerAdapterDb implements PagerAdapterInterface
{
    private $dbAdapter;
    private $config;

    function __construct($classname, $options = null) {
        $this->setDatabase($classname);
        if ($options !== null) {
            $this->config = $options;
        }
    }

    function setDatabase($classname)
    {
        if (!(call_user_func(array($classname, 'getInstance')) instanceof Pagable)) {
            throw new ViewException('Database does not implement interface Pageable');
        }
        $this->dbAdapter = $classname;
    }

    function page($offset, $lenght, $options = null)
    {
        if ($this->dbAdapter === null) {
            return array();
        }
        return call_user_func_array(
            array($this->dbAdapter, 'getPage'),
            array($offset, $lenght, $this->config)
        );
    }

    function count($options = null)
    {
        if ($this->dbAdapter === null) {
            return 0;
        }
        return call_user_func(
            array($this->dbAdapter, 'getCount'),
            $this->config
        );
    }
}