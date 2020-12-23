<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $uri], true)?>

<div class="container">
    <form action="" method="post">
        <a href="/admin/platby/structure/category/add">Přidat specifický symbol</a>
        <?= (new TableHelper($data))
                 ->style('width:100%')
                 ->column('buttons', '', '', 'width:100px')
                 ->column('name', 'Název')
                 ->column('symbol', 'Specifický symbol')
                 ->column('validDate', 'Platí pro období')
                 ->column('amount', 'Částka') ?>
        <br>
        <h2>Archivované symboly</h2>
        <?= (new TableHelper($archived))
                 ->style('width:100%')
                 ->column('buttons', '', '', 'width:100px')
                 ->column('name', 'Název')
                 ->column('symbol', 'Specifický<br/>symbol')
                 ->column('validDate', 'Platí pro období')
                 ->column('amount', 'Částka') ?>
    </form>
</div>
