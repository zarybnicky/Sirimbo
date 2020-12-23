<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', [], true) ?>

<div class="container full">
    <form action="" method="get">
        <span class="big">Omezit výběr:</span>
        <table>
            <tr>
                <td>uživatel</td>
                <td>
                    <?php
                    $s = new \SelectHelper('user', ['all' => '---'], $user ?: 'all');
                    foreach ($users as $user) {
                        $s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
                    }
                    echo $s;
                    ?>
                </td>
            </tr>
            <tr>
                <td>spec. symbol</td>
                <td>
                    <?= new \SelectHelper('category', array_merge(['all' => '---'], $categories), $category ?: 'all') ?>
                </td>
            </tr>
            <tr>
                <td>datum</td>
                <td><?= (new \DateHelper('date', $date))->range() ?></td>
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
  <?= (new TableHelper($data))
         ->style('width:100%')
         ->column('buttons', '')
         ->column('amount', 'Částka', 'r')
         ->column('date', 'Datum')
         ->column('fullName', 'Uživatel')
         ->column('category', 'Kategorie') ?>
</form>
</div>
