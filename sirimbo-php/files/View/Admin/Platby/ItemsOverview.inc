<?= new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container full">
    <form action="" method="get">
        <span class="big">Omezit výběr:</span>
        <table>
            <tr>
                <td>uživatel</td>
                <td>
                    <?php
                    $s = new \SelectHelper('user', ['all' => '---'], $this->user ?: 'all');
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
                    <?= new \SelectHelper('category', array_merge(['all' => '---'], $this->categories), $this->category ?: 'all') ?>
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
                <td><?= new \SubmitHelper('Odeslat') ?></td>
            </tr>
        </table>
    </form>
    <br/>

<form action="" method="POST">
  <?= (new TableHelper($this->data))
         ->style('width:100%')
         ->column('buttons', '')
         ->column('amount', 'Částka', 'r')
         ->column('date', 'Datum')
         ->column('fullName', 'Uživatel')
         ->column('category', 'Kategorie') ?>
</form>
</div>
