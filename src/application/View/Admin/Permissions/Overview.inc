<h1>Správa oprávnění</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo $this->menu()
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