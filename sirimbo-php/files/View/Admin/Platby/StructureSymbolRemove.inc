<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
    <h3>Odstranit specifický symbol?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit specifický symbol "<?= $name ?>"?
        <?= \Utils::hidden('data[]', $id) ?>
        <?= \Utils::hidden('returnURI', $returnURI) ?>
        <br/>
        <?= \Utils::submit('Odstranit', 'action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </form>
</div>
