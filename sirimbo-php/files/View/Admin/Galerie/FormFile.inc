<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Popis</label>
      <?= new \TextHelper('name', $name) ?>
    </div>
    <div class="form-group">
      <label for="parent">Nadsložka</label>
      <?php
      $s = new \SelectHelper('parent', ['0' => '---'], $parent);
      foreach ($dirs as $item) {
          $s->option($item['id'], $item['text']);
      }
      echo $s;
      ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
      <?= new \SubmitHelper('Upravit') ?>
    </div>
  </form>
</div>
