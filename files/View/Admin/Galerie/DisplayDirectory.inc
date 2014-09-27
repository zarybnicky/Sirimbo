<div class="container">
<h1>Správa fotogalerie</h1>
<?php
echo $this->notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<?php
echo $this->menu()
    ->float(MenuHelper::FLOAT_RIGHT)
    ->content('Přidat fotky', '/admin/galerie/file/upload?dir=' . $this->id)
    ->content('Upravit fotku', 'file/edit', true)
    ->content('Odstranit fotky', 'file/remove', true)
    ->content('Zpět', '/admin/galerie');
?>
<?php
if (!count($this->files)) :
    echo $this->notice('Žádné fotky');
else :
?>

<div style="margin-right:150px;">';
<?php foreach ($this->files as $item) : ?>
    <div>
        <div class="f_preview">
            <div class="f_img">
                <img alt="<?= $item['id'] ?>" src="<?= $item['thumbnailURI']?>" />
            </div>
        </div>
        <div class="f_popis">
            <?= $item['checkBox'] ?>
            <?= $item['name'] ?>
        </div>
    </div>
<?php endforeach; ?>
</div>

<?php endif; ?>
</form>
</div>
