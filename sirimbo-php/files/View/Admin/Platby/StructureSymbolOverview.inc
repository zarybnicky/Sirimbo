<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri])?>

<div class="container">
    <form action="" method="post">
        <a href="/admin/platby/structure/category/add">Přidat specifický symbol</a>
        <?= (new TableHelper($this->data))
                 ->style('width:100%')
                 ->column('buttons', '', '', 'width:100px')
                 ->column('name', 'Název')
                 ->column('symbol', 'Specifický symbol')
                 ->column('validDate', 'Platí pro období')
                 ->column('amount', 'Částka') ?>
        <br>
        <h2>Archivované symboly</h2>
        <?= (new TableHelper($this->archived))
                 ->style('width:100%')
                 ->column('buttons', '', '', 'width:100px')
                 ->column('name', 'Název')
                 ->column('symbol', 'Specifický<br/>symbol')
                 ->column('validDate', 'Platí pro období')
                 ->column('amount', 'Částka') ?>
    </form>
</div>
