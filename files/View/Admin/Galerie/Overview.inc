<?= $this->header('Správa fotogalerie') ?>
<div class="container">
  <form action="" method="POST">
    <a class="btn btn-outline-primary" href="/admin/galerie/file/upload">Přidat fotky</a>
    <a class="btn btn-outline-primary" href="/admin/galerie/directory/add">Přidat složku</a>
    <button class="btn btn-outline-primary" name="action" value="scan">Aktualizovat databázi</button>
    <button class="btn btn-outline-primary" name="action" value="save">Uložit změny</button>
    <?= $this->table()
           ->column('buttons', '')
           ->column('name', 'Název')
           ->column('hidden', 'Skrytá')
           ->data($this->data) ?>
  </form>
</div>
