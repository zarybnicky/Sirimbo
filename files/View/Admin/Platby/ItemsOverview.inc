<div class="container">
<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
echo '<h2>Jednotlivé platby</h2>';

if (!TISK) {
    echo '<form action="" method="get">';
    echo '<span class="big">Omezit výběr:</span>';

    echo '<table><tr><td>uživatel</td>';
    echo '<td>';
    $s = $this->select()
        ->get()->name('user')
        ->value('all')
        ->option('all', '---');
    foreach ($this->users as $user)
        $s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
    echo $s;
    echo '</td></tr>';

    echo '<tr><td>spec. symbol</td>';
    echo '<td>';
    $s = $this->select()
        ->get()->name('category')
        ->value('all')
        ->option('all', '---');
    foreach ($this->categories as $key => $item)
        $s->option($key, $item);
    echo $s;
    echo '</td></tr>';

    echo '<tr><td>datum</td>';
    echo '<td>';
    echo (new DateHelper())->date()->name('date')->range()->textBox()->post(false);
    echo '</td></tr>';
    echo '<tr><td></td><td><button type="submit">Odeslat</button></td></tr></table>';
    echo '</form><br/>';
}

echo '<form action="" method="POST">';
if (!TISK) {
    echo $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Přidat', '/admin/platby/items/add')
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true);
}
if ($this->data == array()) {
    echo $this->notice('Žádné takové platby');
    return;
}
echo '<div style="margin-right:150px">';
echo $this->table()
    ->style('width:100%')
    ->data($this->data)
    ->column('checkBox', '')
    ->column('amount', 'Částka', 'r')
    ->column('date', 'Datum')
    ->column('fullName', 'Uživatel')
    ->column('category', 'Kategorie');
echo '</div>';
echo '</form>';
?>
</div>
