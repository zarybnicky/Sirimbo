<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $uri], true) ?>

<div class="container">
    <h3>Odstranit specifický symbol?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit specifický symbol "<?= $name ?>"?
        <?= new \HiddenHelper('data[]', $id) ?>
        <?= new \HiddenHelper('returnURI', $returnURI) ?>
        <br/>
        <?= (new \SubmitHelper('Odstranit'))->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </form>
</div>
