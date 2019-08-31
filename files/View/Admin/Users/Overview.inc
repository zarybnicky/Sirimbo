<div class="container">

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
                <td colspan="2"><?= $this->submit('Filtrovat') ?></td>
            </tr>
        </table>
    </form>

<form action="" method="POST">
    <a href="/admin/users/add" class="btn btn-outline-primary">Nový uživatel</a>
    <a href="/admin/users/duplicate" class="btn btn-outline-primary">Duplicitní uživatelé</a>
    <a href="/admin/users/unconfirmed" class="btn btn-outline-primary">Nepotvrzení uživatelé</a>
    <a href="/admin/users/statistiky" class="btn btn-outline-primary">Statistiky</a>
    <?php if ($this->view == 'status'): ?>
      <br><br>
      <button class="btn btn-primary" name="action" value="save">Uložit změny</button>
    <?php endif ?>
    <br>

<?php
if (!$this->data) {
    echo $this->notice('Žádní takoví uživatelé');
    echo '</div>';
    return;
}
$table = $this->table()
    ->style('width:100%')
    ->data($this->data)
    ->column('checkBox', '')
    ->column('index', '')
    ->column('fullName', 'Jméno');
if ($this->view == 'status') {
    $table->column('skupina', '')
        ->column('groupInfo', 'Oprávnění')
        ->column('system', 'Systémový<br/>uživatel', 'center')
        ->column('ban', 'Ban', 'center');
} else {
    $table->column('colorBox', '')
        ->column('groupInfo', 'Oprávnění')
        ->column('birthDate', 'Datum<br/>narození')
        ->column('varSymbol', 'Variabilní<br/>symbol');
}
echo $table;
?>
</form>
<div style="text-align:center"><?= $this->navigation ?></div>
</div>
