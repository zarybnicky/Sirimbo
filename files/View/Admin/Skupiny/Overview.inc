<?php
header_main('Správa skupin');
notice($this->redirect()->getMessage());

echo '<form action="/admin/skupiny" method="POST">';
if($this->showMenu) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'skupiny/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
echo '<div style="margin-right:150px">';
echo $this->table()
	->style('width:100%')
	->column('buttons', '', 'center', 'width:50px')
	->column('colorBox', 'Barvy', 'center', 'width:45px')
	->column('name', 'Název')
	->data($this->data);
echo '</div>';
echo '</form>';