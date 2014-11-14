<?php
class PagingAdapterDBSelect implements PagingAdapterInterface
{
    private $_dbadapter;
    private $_options;

    public function __construct($classname, $options = null) {
        $this->setDatabase($classname);
        if ($options !== null)
            $this->_options = $options;
    }
    public function setDatabase($classname) {
        if (!(call_user_func(array($classname, 'getInstance')) instanceof Pagable))
            throw new ViewException('Database does not implement interface Pageable');

        $this->_dbadapter = $classname;
    }
    public function page($offset, $lenght, $options = null) {
        return $this->_dbadapter ?
            call_user_func_array(array($this->_dbadapter, 'getPage'),
                array($offset, $lenght, $this->_options))
            : array();
    }
    public function count($options = null) {
        return $this->_dbadapter ?
            call_user_func(array($this->_dbadapter, 'getCount'), $this->_options)
            : 0;
    }
}