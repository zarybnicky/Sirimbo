<div class="container">
  <form action="/admin/dokumenty" method="POST">
    <?= (new TableHelper($this->data))
           ->column('buttons', '')
           ->column('link', 'Jméno souboru')
           ->column('name', 'Soubor')
           ->column('category', 'Kategorie')
           ->column('by', 'Nahrál') ?>
  </form>
  <hr />
  <h2>Upload</h2>
  <form action="" method="post" enctype="multipart/form-data">
    <div class="form-group">
      <label for="name">Název</label>
      <?= (new \TextHelper('name', ''))->placeholder('Název') ?>
    </div>
    <div class="form-group">
      <input type="file" class="form-control-file" name="file">
    </div>
    <div class="form-group">
      <label for="kategorie">Kategorie</label>
      <select class="form-control" name="kategorie">
        <option value="1">Schůze, rady</option>
        <option value="2">Soutěže</option>
        <option value="3">Tábory</option>
        <option value="0">Ostatní</option>
      </select>
    </div>
    <div class="form-group">
      <?= $this->submit('Nahrát')->data('action', 'upload') ?>
    </div>
  </form>
</div>
