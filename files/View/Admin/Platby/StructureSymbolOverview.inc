<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri])?>
<?= $this->subheader('Specifické symboly') ?>

<div class="container">
    <form action="" method="post">
        <a href="/admin/platby/structure/category/add">Přidat specifický symbol</a>
        <?= $this->table()
                 ->style('width:100%')
                 ->column('buttons', '', '', 'width:80px')
                 ->column('name', 'Název')
                 ->column('symbol', 'Specifický symbol')
                 ->column('validDate', 'Platí pro období')
                 ->column('amount', 'Částka')
                 ->data($this->data) ?>
        <br>
        <h2>Archivované symboly</h2>
        <?= $this->table()
                 ->style('width:100%')
                 ->column('buttons', '', '', 'width:80px')
                 ->column('name', 'Název')
                 ->column('symbol', 'Specifický<br/>symbol')
                 ->column('validDate', 'Platí pro období')
                 ->column('amount', 'Částka')
                 ->data($this->archived) ?>
    </form>
</div>
