<?= $this->header($this->header) ?>
<div class="container full">
    <form action="" method="POST">
        <?= $this->prompt;?><br/><br/>
        <?php foreach ($this->data as $row): ?>
            <?= $row['text']?>
            <input type="hidden" name="data[]" value="<?= $row['id']?>" />
            <br />
        <?php endforeach;?>
        <br/>
        <?= $this->submit('Odstranit')->data('action', 'confirm') ?>
        <a href="<?= $this->returnURI;?>">Zpět</a>
    </form>
</div>
