<div class="container">
    <form action="" method="POST">
        <?= $prompt ?><br/><br/>
        <?php foreach ($data as $row) : ?>
            <?= $row['text']?>
            <?= \Utils::hidden('data[]', $row['id']) ?>
            <br />
        <?php endforeach ?>
        <br/>
        <?= \Utils::submit('Odstranit', 'action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </form>
</div>
