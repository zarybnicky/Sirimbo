<div class="container full">
<h1>Správa akcí</h1>
<form action="" method="POST">
<?php
echo $this->notice($this->redirect()->getMessage());
if (!TISK) {
    echo $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Přidat', 'akce/add')
        ->content('Upravit', 'edit', true)
        ->content('Upravit přihlášené', 'detail', true)
        ->content('Upravit dokumenty', 'dokumenty', true)
        ->content('Odstranit', 'remove', true)
        ->content('Uložit změny', 'save', true);
    echo '<div style="margin-right:150px;">';
} else {
    echo '<div>';
}
echo $this->table()
    ->style("width:100%")
    ->column('checkBox', '')
    ->column('name', 'Jméno')
    ->column('place', 'Místo')
    ->column('date', 'Datum')
    ->column('userCount', 'Účastníci')
    ->column('visible', 'Viditelný')
    ->data($this->data);
echo '</div>';
?>
</form>
</div>
