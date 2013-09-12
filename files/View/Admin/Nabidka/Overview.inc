<?php
header_main('Správa nabídka');
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if($this->showMenu) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'nabidka/add')
		->content('Upravit', 'edit', true)
		->content('Upravit tréninky', 'edit_detail', true)
		->content('Odstranit', 'remove', true)
		->content('Uložit změny', 'save', true);
}
echo '<div style="margin-right:150px;">';
echo $this->table()
	->style('width:100%;')
	->data($this->data)
	->column('checkBox', '')
	->column('fullName', 'Trenér')
	->column('date', 'Datum')
	->column('visible', 'Viditelný');
echo '</div>';
echo '</form>';
?>