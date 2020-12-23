<?php
class TableHelper
{
    private $_name;
    private $_data;
    private $_columns;
    private $_index;
    private $_style;
    private $_showHeader;

    public function __construct($data)
    {
        $this->_name = '';
        $this->_data = $data;
        $this->_columns = [];
        $this->_index = 0;
        $this->_style = '';
        $this->_showHeader = true;
    }

    public function name($n)
    {
        $this->_name = $n;
        return $this;
    }

    public function style($s)
    {
        $this->_style = ' style="' . $s . '"';
        return $this;
    }

    public function data($d)
    {
        if ($d instanceof Traversable || (is_array($d) && is_array(reset($d)))) {
            $this->_data = $d;
        }
        return $this;
    }

    public function columns($columns, $overwrite = false)
    {
        if ($overwrite) {
            $this->_columns = [];
        }
        if (!is_array($columns)) {
            return $this;
        }
        foreach ($columns as $c) {
            if (!is_array($c)) {
                continue;
            }
            call_user_func_array([$this, 'column'], $c);
        }
        return $this;
    }

    public function column($id, $name, $class = null, $style = null)
    {
        $html = ($class !== null ? (' class="' . $class . '"') : '')
                . ($style !== null ? (' style="' . $style . '"') : '');

        $this->_columns[] = [$id, $name, $html];
        return $this;
    }

    public function showHeader($b)
    {
        $this->_showHeader = (bool) $b;
        return $this;
    }

    private function _getCounter()
    {
        return ' ' . $this->_index . '.';
    }

    public function __toString()
    {
        if ($this->_data === null && $this->_columns === null) {
            return '';
        }
        ob_start();
        ?>
<table<?= $this->_style ?>>
    <?php if ($this->_showHeader) : ?>
    <thead>
        <tr>
            <?php foreach ($this->_columns as $c): ?>
            <th><?= $c[1] ?></th>
            <?php endforeach ?>
        </tr>
    </thead>
    <?php endif ?>
    <tbody>
        <?php foreach ($this->_data as $row): if (!$row) continue ?>
        <tr>
            <?php foreach ($this->_columns as $c): ?>
            <td<?= $c[2] ?>><?= $row[$c[0]] ?></td>
            <?php endforeach ?>
        </tr>
        <?php endforeach ?>
    </tbody>
</table>
        <?php
        return ob_get_clean();
    }
}
