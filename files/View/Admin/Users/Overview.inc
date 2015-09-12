<?= $this->header('Správa uživatelů') ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage()) ?>

<?php if ($this->showMenu): ?>
    <form action="" method="get">
        <table>
            <tr>
                <td>Uživatelé:</td>
                <td>
                    <?= $this->select('status')
                             ->set($this->status ?: 'all')
                             ->options($this->statusOptions) ?>
                </td>
            </tr>
            <tr>
                <td>Skupina:</td>
                <td>
                    <?= $this->select('skupina')
                             ->set($this->skupina ?: 'all')
                             ->options($this->skupinyOptions) ?>
                </td>
            </tr>
            <tr>
                <td>Oprávnění:</td>
                <td>
                    <?= $this->select('group')
                             ->set($this->group ?: 'all')
                             ->options($this->groupOptions) ?>
                </td>
            </tr>
            <tr>
                <td>Řadit podle:</td>
                <td>
                    <?= $this->select('sort')
                             ->set($this->sort ?: 'prijmeni')
                             ->options($this->sortOptions) ?>
                </td>
            </tr>
            <tr>
                <td>Zobrazení:</td>
                <td>
                    <?= $this->select('view')
                             ->set($this->view ?: 'info')
                             ->option('info', 'informace')
                             ->option('status', 'status uživatele') ?>
                </td>
            </tr>
            <tr>
                <td><?= $this->submit('Odeslat') ?></td>
                <td></td>
            </tr>
        </table>
    </form>
    <br>
<?php endif ?>

<form action="" method="POST">

<?php
if ($this->showMenu) {
    $f = $this->menu()
        ->content('Přidat', '/admin/users/add')
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true)
        ->content('Zobrazit platby', 'platby', true)
        ->content('Duplicitní uživatelé', '/admin/users/duplicate')
        ->content('Nepotvrzení uživatelé', '/admin/users/unconfirmed')
        ->content('Statistiky', '/admin/users/statistiky');
    if ($this->view == 'status') {
        $f->content('Uložit změny', 'save', true);
    }
    echo $f;
}
if (!$this->data) {
    echo $this->notice('Žádní takoví uživatelé');
    echo '</div>';
    return;
}
echo '<div style="margin-right:150px">';

$table = $this->table()
    ->style('width:100%')
    ->data($this->data)
    ->column('checkBox', '')
    ->column('index', '')
    ->column('fullName', 'Jméno');
if ($this->view == 'status') {
    $table->column('skupina', '')
        ->column('groupInfo', 'Oprávnění')
        ->column('dancer', 'Aktivní<br/>tanečník', 'center')
        ->column('system', 'Systémový<br/>uživatel', 'center')
        ->column('ban', 'Ban', 'center');
} else {
    $table->column('colorBox', '')
        ->column('groupInfo', 'Oprávnění')
        ->column('birthDate', 'Datum<br/>narození')
        ->column('varSymbol', 'Variabilní<br/>symbol');
}
echo $table;

echo '</div>';
echo '</form>';
echo '<div style="text-align:center;margin-right:150px;">', $this->navigation, '</div>';
?>
</div>
