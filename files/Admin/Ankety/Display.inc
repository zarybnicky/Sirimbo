<h1>Správa anket</h1>
<?php echo $this->notice($this->redirect()->getMessage()); ?>
<form action="" method="POST">
<?php
if (!TISK) {
    echo $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Přidat', 'ankety/add')
        ->content('Upravit', 'edit', true)
        ->content('Odstranit', 'remove', true)
        ->content('Uložit', 'save', true);
}
?>
<table>
<tr>
<td></td>
<td>Jméno</td>
<td>Viditelný</td>
</tr>
<?php
$ankety = DBAnkety::getAnkety();
foreach ($ankety as $item) {
    echo '<tr>';

    echo '<td>',
        $this->checkbox('ankety[]', $item['ak_id'])
             ->disabled(Permissions::check('ankety', P_OWNED,$item['ak_kdo'])),
        '</td>';
    echo '<td>', $item['ak_jmeno'], '</td>';
    echo '<td>',
        $this->checkbox($item['ak_id'], '1')
             ->defaultState($item['ak_visible']),
        '</td>';
    echo '</tr>';
}
?>
</table>
</form>
