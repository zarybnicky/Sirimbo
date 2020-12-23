<div class="container">
    <form action="" method="POST">
        <?= $prompt ?><br/><br/>
        <?php foreach ($data as $row) : ?>
            <?= $row['text']?>
            <?= new \HiddenHelper('data[]', $row['id']) ?>
            <br />
        <?php endforeach ?>
        <br/>
        <?= (new \SubmitHelper('Odstranit'))->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
    </form>
</div>
