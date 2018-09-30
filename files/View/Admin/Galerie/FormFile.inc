<?= $this->header('Správa fotogalerie', 'Upravit soubor') ?>
<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Popis</label>
      <?= $this->text('name', $this->name) ?>
    </div>
    <div class="form-group">
      <label for="parent">Nadsložka</label>
      <?php
      $s = $this->select('parent')
               ->set($this->parent)
               ->option('0', '--');
      foreach ($this->dirs as $item) {
          $s->option($item['id'], $item['text']);
      }
      echo $s;
      ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="<?= $this->referer ?>">Zpět</a>
      <?= $this->submit('Upravit') -> data('action', 'add') ?>
    </div>
  </form>
</div>
