<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Kategorie plateb</h2>';

echo '<span class="big">Přidat: </span> ';
echo '<a href="/admin/platby/structure/group/add">kategorii</a>&nbsp;';

echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('buttons', '', '', 'width:50px')
    ->column('name', 'Název')
    ->column('type', 'Typ platby')
    ->data($this->data);
