<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Jednotlivé platby</h2>';

if (!TISK) {
    echo '<form action="" method="get">';
    echo '<span class="big">Omezit výběr podle uživatele:</span> ';
    $select = (new TKOlomouc\View\Helper\Select('user'))
        ->get()
        ->value('all')
        ->option('all', '---');
    foreach ($this->users as $user)
        $select->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
    echo $select;
    echo '<button type="submit">Odeslat</button>';
    echo '</form>';
}

echo '<form action="" method="POST">';
if (!TISK) {
    echo (new TKOlomouc\View\Helper\Menu())
        ->content('Přidat', '/admin/platby/items/add')
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true);
}
if ($this->data == array()) {
    notice('Žádné takové platby');
    return;
}
echo '<div style="margin-right:150px">';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->column('checkBox', '')
    ->column('amount', 'Částka', 'r')
    ->column('date', 'Datum')
    ->column('fullName', 'Uživatel')
    ->column('category', 'Kategorie')
    ->data($this->data);
echo '</div>';
echo '</form>';
