<h1>Správa nabídky</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo $this->menu()
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