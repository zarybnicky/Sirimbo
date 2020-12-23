<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
    <h3>Odstranit kategorii?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit kategorii plateb "<?= $name ?>"?
        <?= new \HiddenHelper('data[]', $id) ?>
        <?= new \HiddenHelper('returnURI', $returnURI) ?>
        <br/>
        <?= (new \SubmitHelper('Odstranit'))->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </form>
</div>
