<?php
namespace TKOlomouc\View\Helper;

class Table
{
    private $_name;
    private $_data;
    private $_columns;
    private $_index;
    private $_style;
    private $_showHeader;

    function table() {
        $this->_defaultValues();
        return $this;
    }
    private function _defaultValues() {
        $this->_name = '';
        $this->_data = array();
        $this->_columns = array();
        $this->_index = 0;
        $this->_style = '';
        $this->_showHeader = true;
    }
    function name($n) {
        $this->_name = $n;
        return $this;
    }
    function style($s) {
        $this->_style = ' style="' . $s . '"';
        return $this;
    }
    function data($d) {
        if ($d instanceof Traversable || (is_array($d) && is_array(reset($d)))) {
            $this->_data = $d;
        }
        return $this;
    }
    function columns($columns, $overwrite = false) {
        if ($overwrite) {
            $this->_columns = null;
        }
        if (!is_array($columns)) {
            return $this;
        }
        foreach ($columns as $c) {
            if (!is_array($c)) {
                continue;
            }
            call_user_func_array(array($this, 'column'), $c);
        }
        return $this;
    }
    function column($id, $name, $class = null, $style = null) {
        $html = ($class !== null ? (' class="' . $class . '"') : '')
                . ($style !== null ? (' style="' . $style . '"') : '');

        $this->_columns[] = array($id, $name, $html);
        return $this;
    }
    function showHeader($b) {
        $this->_showHeader = (bool) $b;
        return $this;
    }
    private function _getCounter() {
        return ' ' . $this->_index . '.';
    }
    function __toString() {
        return $this->render();
    }
    function render() {
        if ($this->_data === null && $this->_columns === null) {
            return '';
        }
        ob_start();
        ?>
<table<?php echo $this->_style;?>>
    <?php if ($this->_showHeader) : ?>
    <thead>
        <tr>
            <?php foreach ($this->_columns as $c): ?>
            <th><?php echo $c[1];?></th>
            <?php endforeach;?>
        </tr>
    </thead>
    <?php endif;?>
    <tbody>
        <?php foreach ($this->_data as $row): ++$this->_index; if (!$row) continue;?>
        <tr>
            <?php foreach ($this->_columns as $c): ?>
            <td<?php echo $c[2];?>>
            <?php echo $row[$c[0]] == '{counter}' ? $this->_getCounter() : $row[$c[0]];?>
            </td>
            <?php endforeach;?>
        </tr>
        <?php endforeach;?>
    </tbody>
</table>
        <?php
        return ob_get_clean();
    }
}
?>