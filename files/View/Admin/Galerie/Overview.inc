<div class="container full">
<h1>Správa fotogalerie</h1>
<?php echo $this->notice($this->redirect()->getMessage()); ?>
<form action="" method="POST">
<?php
echo $this->menu()
    ->float(MenuHelper::FLOAT_RIGHT)
    ->content('Přidat fotky', '/admin/galerie/file/upload')
    ->content('Přidat složku', '/admin/galerie/directory/add')
    ->content('Upravit fotky', 'directory', true)
    ->content('Upravit složku', 'directory/edit', true)
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
