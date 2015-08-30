<?= $this->header('Správa článků') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()) ?>
    <form action="" method="get">
        <?= $this->select('dir')
                 ->set($this->dir ?: 'none')
                 ->option('none', '------ vyberte složku ------')
                 ->options($this->dirs)
        ?>
        <?= $this->submit('Zobrazit') ?>
    </form>
    <form action="" method="post">
        <?php foreach ($this->photos as $item): ?>
            <div class="foto_preview">
                <div class="foto_preview_in">
                    <img alt="<?= $item['id'] ?>" src="<?= $item['src'] ?>" />
                </div>
                <div class="foto_desc">
                    <?= $this->radio('foto', $item['id'])
                             ->set($item['id'] == $this->checked) ?>
                    <br/><br/>
                </div>
            </div>
        <?php endforeach; ?>
        <div style="clear:both">
            <?= $this->submit('Potvrdit') ?>
            <a href="/admin/aktuality">Zpět</a>
        </div>
    </form>
</div>
