<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
    <h3>Odstranit kategorii?</h3>
    <form action="" method="POST">
        Opravdu chcete odstranit kategorii plateb "<?= $name ?>"?
        <?= \Utils::hidden('data[]', $id) ?>
        <?= \Utils::hidden('returnURI', $returnURI) ?>
        <br/>
        <?= \Utils::submit('Odstranit', 'action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </form>
</div>
