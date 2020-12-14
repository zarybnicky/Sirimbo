<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
    <h3>Odstranit kategorii?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit kategorii plateb "<?= $this->name ?>"?
        <?= new \HiddenHelper('data[]', $this->id) ?>
        <?= new \HiddenHelper('returnURI', $this->returnURI) ?>
        <br/>
        <?= (new \SubmitHelper('Odstranit'))->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a>
    </form>
</div>
