<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Jednotlivé platby') ?>

<div class="container full">
<?php if (!TISK): ?>
    <form action="" method="get">
        <span class="big">Omezit výběr:</span>
        <table>
            <tr>
                <td>uživatel</td>
                <td>
                    <?php
                    $s = $this->select('user')
                              ->set($this->user ?: 'all')
                              ->option('all', '---');
                    foreach ($this->users as $user) {
                        $s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
                    }
                    echo $s;
                    ?>
                </td>
            </tr>
            <tr>
                <td>spec. symbol</td>
                <td>
                    <?= $this->select('category')
                             ->set($this->category ?: 'all')
                             ->option('all', '---')
                             ->options($this->categories);
                    ?>
                </td>
            </tr>
            <tr>
                <td>datum</td>
                <td><?= $this->date('date', $this->date)->range()->textBox() ?></td>
            </tr>
            <tr>
                <td></td>
                <td><?= $this->submit('Odeslat') ?></td>
            </tr>
        </table>
    </form>
    <br/>
<?php endif; ?>

<form action="" method="POST">
<?php
if (!TISK) {
    echo $this->menu()
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
