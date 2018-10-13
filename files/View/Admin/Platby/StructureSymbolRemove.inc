<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
    <h3>Odstranit specifický symbol?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit specifický symbol "<?= $this->name ?>"?
        <?= $this->hidden('data[]', $this->id) ?>
        <?= $this->hidden('returnURI', $this->returnURI) ?>
        <br/>
        <?= $this->submit('Odstranit')->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a>
    </form>
</div>
