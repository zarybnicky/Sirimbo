<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Specifické symboly</h2>';

echo '<span class="big">Přidat: </span> ';
echo '<a href="/admin/platby/structure/category/add">specifický symbol</a>';

echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('buttons', '', '', 'width:50px')
    ->column('name', 'Název')
    ->column('symbol', 'Specifický<br/>symbol')
    ->column('validDate', 'Platí pro období')
    ->column('amount', 'Částka')
    ->data($this->data);

echo '<h2>Archivované symboly</h2>';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('buttons', '', '', 'width:50px')
    ->column('name', 'Název')
    ->column('symbol', 'Specifický<br/>symbol')
    ->column('validDate', 'Platí pro období')
    ->column('amount', 'Částka')
    ->data($this->archived);
