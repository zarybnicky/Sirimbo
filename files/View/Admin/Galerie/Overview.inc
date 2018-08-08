<?= $this->header('Správa fotogalerie') ?>
<div class="container">
<form action="" method="POST">
<?php
echo $this->menu()
    ->content('Přidat fotky', '/admin/galerie/file/upload')
    ->content('Přidat složku', '/admin/galerie/directory/add')
    ->content('Upravit fotky', 'directory', true)
    ->content('Přejmenovat složku', 'directory/edit', true)
    ->content('Odebrat složku', 'directory/remove', true)
    ->content('Aktualizovat databázi', 'galerie/scan', true)
    ->content('Uložit změny', 'galerie/save', true);

echo $this->table()
    ->column('checkBox', '')
    ->column('name', 'Název')
    ->column('hidden', 'Skrytá')
    ->data($this->data);
?>
</form>
</div>
