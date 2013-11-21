<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Vyřazené platby (' . $this->header . ')</h2>';

$table = (new TKOlomouc\View\Helper\Table())
    ->data($this->data);
foreach ($this->columns as $item) {
    $table->column($item[0], $item[1]);
}
echo $table;