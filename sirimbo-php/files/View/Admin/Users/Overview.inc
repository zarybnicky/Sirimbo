<div class="container">

    <form action="" method="get">
        <table>
            <tr>
                <td>Uživatelé:</td>
                <td>
                    <?= new \SelectHelper('status', $statusOptions, $status ?: 'all') ?>
                </td>
            </tr>
            <tr>
                <td>Skupina:</td>
                <td>
                    <?= new \SelectHelper('skupina', $skupinyOptions, $skupina ?: 'all') ?>
                </td>
            </tr>
            <tr>
                <td>Oprávnění:</td>
                <td>
                    <?= new \SelectHelper('group', $groupOptions, $group ?: 'all') ?>
                </td>
            </tr>
            <tr>
                <td>Řadit podle:</td>
                <td>
                    <?= new \SelectHelper('sort', $sortOptions, $sort ?: 'prijmeni') ?>
                </td>
            </tr>
            <tr>
                <td>Zobrazení:</td>
                <td>
                    <?= new \SelectHelper('view', ['info' => 'informace', 'status' => 'status uživatele'], $view ?: 'info') ?>
                </td>
            </tr>
            <tr>
                <td colspan="2"><?= new \SubmitHelper('Filtrovat') ?></td>
            </tr>
        </table>
    </form>

<form action="" method="POST">
    <a href="/admin/users/add" class="btn btn-outline-primary">Nový uživatel</a>
    <a href="/admin/users/duplicate" class="btn btn-outline-primary">Duplicitní uživatelé</a>
    <a href="/admin/users/unconfirmed" class="btn btn-outline-primary">Nepotvrzení uživatelé</a>
    <a href="/admin/users/statistiky" class="btn btn-outline-primary">Statistiky</a>
    <a href="/admin/users/getMsmtCsv" class="btn btn-outline-primary">MŠMT export</a>
    <?php if ($view == 'status'): ?>
      <button class="btn btn-primary" name="action" value="save">Uložit změny</button>
    <?php endif ?>
    <br>

<?php
if (!$data) {
    echo new \NoticeHelper('Žádní takoví uživatelé');
    echo '</div>';
    return;
}
$table = (new TableHelper($data))
    ->style('width:100%')
    ->column('checkBox', '')
    ->column('index', '')
    ->column('fullName', 'Jméno');
if ($view == 'status') {
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
<div style="text-align:center"><?= $navigation ?></div>
</div>
