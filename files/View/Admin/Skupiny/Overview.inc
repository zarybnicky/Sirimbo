<h1>Správa skupin</h1>
<?php
echo $this->notice($this->redirect()->getMessage());

echo '<span class="big">Přidat: </span> ';
echo '<a href="/admin/skupiny/add">kategorii</a>&nbsp;';

echo '<form action="/admin/skupiny" method="POST">';
echo '<div style="margin-right:150px">';
echo $this->table()
    ->style('width:100%')
    ->column('buttons', '', 'center', 'width:50px')
    ->column('colorBox', 'Barvy', 'center', 'width:45px')
    ->column('name', 'Název')
    ->data($this->data);
echo '</div>';
echo '</form>';
