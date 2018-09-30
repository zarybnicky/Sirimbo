<?= $this->header('Správa fotogalerie', 'Upload') ?>
<div class="container">
  <form enctype="multipart/form-data" action="" method="post">
    <div class="form-group">
      <?= $this->upload('files') ?>
    </div>
    <div class="form-group">
      <label for="dir">Složka</label>
      <?php
      $s = $this->select('dir')->set($this->dir);
      foreach ($this->dirs as $item) {
          $s->option($item['id'], $item['text']);
      }
      echo $s;
      ?>
    </div>
    <div class="form-group">
      <?= $this->submit('Nahrát na server')i ?>
      <a class="btn btn-outline-primary" href="/admin/galerie">Zpět</a>
    </div>
  </form>
</div>
