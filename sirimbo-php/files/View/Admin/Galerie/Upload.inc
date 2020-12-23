<div class="container">
  <form enctype="multipart/form-data" action="" method="post">
    <div class="form-group">
      <?= new UploadHelper('files') ?>
    </div>
    <div class="form-group">
      <label for="dir">Složka</label>
      <?php
      $s = new \SelectHelper('dir', [], $dir);
      foreach ($dirs as $item) {
          $s->option($item['id'], $item['text']);
      }
      echo $s;
      ?>
    </div>
    <div class="form-group">
      <?= new \SubmitHelper('Nahrát na server') ?>
      <a class="btn btn-outline-primary" href="/admin/galerie">Zpět</a>
    </div>
  </form>
</div>
