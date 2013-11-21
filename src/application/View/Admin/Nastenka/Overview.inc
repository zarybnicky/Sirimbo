<h1>Správa nástěnky</h1>
<?php
notice($this->redirect()->getMessage());

echo '<form action="" method="POST">';
if ($this->showMenu) {
    echo (new TKOlomouc\View\Helper\Menu())
        ->content('Přidat', '/admin/nastenka/add')
        ->content('Upravit', 'edit', true)
        ->content('Odstranit', 'remove', true);
}
if (empty($this->data)) {
    notice('Žádné položky za toto období');
    return;
}
echo '<div style="margin-right:150px;">';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('checkBox', '')
    ->column('header', 'Nadpis')
    ->column('groups', 'Skupiny')
    ->column('fullName', 'Přidal')
    ->column('timestampAdd', 'Přidáno', '', 'width:83px;')
    ->column('timestampEdit', 'Naposledy<br/>upraveno', '', 'width:83px')
    ->data($this->data);
echo '</div>';
echo '</form>';
?>
<div style="margin-right:150px;text-align:center;"><?php echo $this->navigation;?></div>