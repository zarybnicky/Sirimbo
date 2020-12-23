<div class="container">
  <form enctype="multipart/form-data" action="" method="post">
    <div class="form-group">
      <?= new UploadHelper('files') ?>
    </div>
    <div class="form-group">
      <label for="dir">Složka</label>
      <?= \Utils::selectAssoc('dir', $dirs, 'id', 'text', $dir) ?>
    </div>
    <div class="form-group">
      <?= \Utils::submit('Nahrát na server') ?>
      <a class="btn btn-outline-primary" href="/admin/galerie">Zpět</a>
    </div>
  </form>
</div>
