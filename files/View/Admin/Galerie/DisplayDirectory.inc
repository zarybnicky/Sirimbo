<?= $this->header('Správa fotogalerie') ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage()) ?>
<form action="" method="POST">
    <?= $this->menu()
             ->content('Přidat fotky', '/admin/galerie/file/upload?dir=' . $this->id)
             ->content('Upravit fotku', 'file/edit', true)
             ->content('Odstranit fotky', 'file/remove', true)
             ->content('Zpět', '/admin/galerie'); ?>
    <?php
    if (!count($this->files)) {
        echo $this->notice('Žádné fotky');
    } else {
    ?>
        <div style="margin-right:150px;">
            <?php foreach ($this->files as $item) : ?>
                <div class="foto_preview">
                    <div class="foto_preview_in">
                        <img alt="<?= $item['id'] ?>" src="<?= $item['thumbnailURI']?>" />
                    </div>
                    <div class="foto_desc">
                        <?= $item['name'] ?>
                    </div>
                </div>
            <?php endforeach; ?>
        </div>
    <?php } ?>
</form>
</div>
