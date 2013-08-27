<?php
header_main('Správa oprávnění');
notice($this->redirect()->getRedirectMessage());

echo '<form action="" method="POST">';
if($this->showMenu) {
	echo $this->menu()
		->float(MenuHelper::FLOAT_RIGHT)
		->content('Přidat', 'permissions/add')
		->content('Upravit', 'edit', true)
		->content('Odebrat', 'remove', true);
}
echo '<div style="margin-right:150px">';
echo $this->table()
	->style('width:100%')
	->data($this->data)
	->column('checkBox', '')
	->column('name', 'Jméno')
	->column('description', 'Popis');
echo '</div>';
echo '</form>';
?>