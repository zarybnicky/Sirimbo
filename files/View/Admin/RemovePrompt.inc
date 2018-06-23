<?= $this->header($this->header) ?>
<div class="container full">
    <form action="" method="POST">
        <?= $this->prompt;?><br/><br/>
        <?php foreach ($this->data as $row): ?>
            <?= $row['text']?>
            <?= $this->hidden('data[]', $row['id']) ?>
            <br />
        <?php endforeach;?>
        <br/>
        <?= $this->submit('Odstranit')->data('action', 'confirm') ?>
        <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a>
    </form>
</div>
