<h1>Správa nabídky</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo (new TKOlomouc\View\Helper\Menu())
        ->content('Přidat', 'nabidka/add')
        ->content('Upravit', 'edit', true)
        ->content('Upravit tréninky', 'edit_detail', true)
        ->content('Odstranit', 'remove', true)
        ->content('Uložit změny', 'save', true);
}
echo '<div style="margin-right:150px;">';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%;')
    ->column('checkBox', '')
    ->column('fullName', 'Trenér')
    ->column('date', 'Datum')
    ->column('visible', 'Viditelný')
    ->data($this->data);
echo '</div>';
echo '</form>';
?>