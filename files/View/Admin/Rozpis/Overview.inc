<h1>Správa rozpisů</h1>
<?php
echo $this->notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Přidat', 'rozpis/add')
        ->content('Upravit', 'edit', true)
        ->content('Upravit tréninky', 'edit_detail', true)
        ->content('Odstranit', 'remove', true)
        ->content('Uložit změny', 'save', true);
}
echo '<div style="margin-right:150px;">';
echo $this->table()
    ->style('width:100%')
    ->column('checkBox', '')
    ->column('fullName', 'Trenér')
    ->column('datum', 'Datum')
    ->column('kde', 'Místo')
    ->column('visible', 'Viditelný')
    ->data($this->data);
echo '</div>';
echo '</form>';
?>
