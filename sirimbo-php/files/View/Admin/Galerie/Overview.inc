<div class="container">
  <form action="" method="POST">
    <a class="btn btn-outline-primary" href="/admin/galerie/file/upload">Přidat fotky</a>
    <a class="btn btn-outline-primary" href="/admin/galerie/directory/add">Přidat složku</a>
    <button class="btn btn-outline-primary" name="action" value="save">Uložit změny</button>
    <?= (new TableHelper($this->data))
           ->column('buttons', '')
           ->column('name', 'Název')
           ->column('hidden', 'Skrytá') ?>
  </form>
</div>
