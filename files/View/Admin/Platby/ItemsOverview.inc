<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

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
                             ->options($this->categories) ?>
                </td>
            </tr>
            <tr>
                <td>datum</td>
                <td><?= $this->date('date', $this->date)->range() ?></td>
            </tr>
            <tr>
                <td><a href="/admin/platby/items/add" class="btn btn-primary">
                  Nová platba
                </a></td>
                <td><?= $this->submit('Odeslat') ?></td>
            </tr>
        </table>
    </form>
    <br/>
<?php endif ?>

<form action="" method="POST">
  <?= $this->table()
         ->style('width:100%')
         ->data($this->data)
         ->column('buttons', '')
         ->column('amount', 'Částka', 'r')
         ->column('date', 'Datum')
         ->column('fullName', 'Uživatel')
         ->column('category', 'Kategorie') ?>
</form>
</div>
