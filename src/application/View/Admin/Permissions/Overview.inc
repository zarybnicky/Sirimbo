<h1>Správa oprávnění</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo (new TKOlomouc\View\Helper\Menu())
        ->content('Přidat', 'permissions/add')
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true);
}
echo '<div style="margin-right:150px">';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('checkBox', '')
    ->column('name', 'Jméno')
    ->column('description', 'Popis')
    ->data($this->data);
echo '</div>';
echo '</form>';
?>