<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Popis</label>
      <?= new \TextHelper('name', $this->name) ?>
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
      <a class="btn btn-outline-primary" href="<?= $this->returnURI ?>">Zpět</a>
      <?= new \SubmitHelper('Upravit') ?>
    </div>
  </form>
</div>
