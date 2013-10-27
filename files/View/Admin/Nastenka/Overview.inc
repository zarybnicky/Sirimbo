<h1>Správa nástěnky</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if($this->showMenu) {
    echo $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Přidat', '/admin/nastenka/add')
        ->content('Upravit', 'edit', true)
        ->content('Odstranit', 'remove', true);
}
if(empty($this->data)) {
    notice('Žádné položky za toto období');
    return;
}
echo '<div style="margin-right:150px;">';
echo $this->table()
    ->style('width:100%')
    ->data($this->data)
    ->column('checkBox', '')
    ->column('header', 'Nadpis')
    ->column('groups', 'Skupiny')
    ->column('fullName', 'Přidal')
    ->column('timestampAdd', 'Přidáno', '', 'width:83px;')
    ->column('timestampEdit', 'Naposledy<br/>upraveno', '', 'width:83px');
echo '</div>';
echo '</form>';
?>
<div style="margin-right:150px;text-align:center;"><?php echo $this->navigation;?></div>