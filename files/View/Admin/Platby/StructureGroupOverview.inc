<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
header_main('Kategorie plateb');

echo '<span class="big">Přidat: </span> ';
echo '<a href="/admin/platby/structure/group/add">kategorii</a>&nbsp;';

echo $this->table()
->style('width:100%')
->column('buttons', '', '', 'width:50px')
->column('name', 'Název')
->column('type', 'Typ platby')
->data($this->data);