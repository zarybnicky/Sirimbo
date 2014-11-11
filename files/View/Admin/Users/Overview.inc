<?= $this->header('Správa uživatelů') ?>
<div class="container full">
<?php
echo $this->notice($this->redirect()->getMessage());

if ($this->showMenu) {
    echo '<form action="" method="get">';
    $s = $this->select();
    echo 'Uživatelé:&nbsp;';
    $s->get()->name('f')
        ->value('all')
        ->option('all', 'všichni')
        ->option('dancer', 'tanečníci')
        ->option('system', 'systémoví')
        ->option('ban', 'zabanovaní');
    foreach ($this->groups as $group) {
        if ($group['id']) {
            $s->option($group['id'], $group['name']);
        }
    }
    echo $s;
    echo '&nbsp;řadit podle:&nbsp;';
    echo $s->select()
        ->get()->name('s')
        ->option('surname', 'příjmení')
        ->option('var-symbol', 'var. symbolu')
        ->option('narozeni', 'data narození');
    echo '&nbsp;zobrazení:&nbsp;';
    echo $s->select()
        ->get()->name('v')
        ->option('info', 'informace')
        ->option('status', 'status uživatele');
    echo '<button type="submit">Odeslat</button>';
    echo '</form><br/>';
}

echo '<form action="" method="POST">';
if ($this->showMenu) {
    $f = $this->menu()
        ->float(MenuHelper::FLOAT_RIGHT)
        ->content('Přidat', '/admin/users/add')
        ->content('Upravit', 'edit', true)
        ->content('Odebrat', 'remove', true)
        ->content('Zobrazit platby', 'platby', true)
        ->content('Duplicitní uživatelé', '/admin/users/duplicate')
        ->content('Nepotvrzení uživatelé', '/admin/users/unconfirmed')
        ->content('Statistiky', '/admin/users/statistiky');
    if (get('v') == 'status') {
        $f->content('Uložit změny', 'save', true);
    }
    echo $f;
}
if ($this->data == array()) {
    echo $this->notice('Žádní takoví uživatelé');
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
        ->column('system', 'Systémový<br/>uživatel', 'center');
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
