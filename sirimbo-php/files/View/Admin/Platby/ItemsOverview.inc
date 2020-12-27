<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container full">
    <form action="" method="get">
        <span class="big">Omezit výběr:</span>
        <table>
            <tr>
                <td>uživatel</td>
                <td><?= \Utils::selectAssoc('user', ['id' => 'all', 'text' => '---'] + $users, 'id', 'text', $user ?: 'all') ?></td>
            </tr>
            <tr>
                <td>spec. symbol</td>
                <td><?= \Utils::select('category', ['all' => '---'] + $categories, $category ?: 'all') ?></td>
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
                <td><button class="btn btn-primary">Odeslat</button></td>
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
