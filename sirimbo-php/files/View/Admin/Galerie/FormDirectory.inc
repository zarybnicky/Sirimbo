<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Název</label>
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
    <div class="form-check">
      <?= new \CheckboxHelper('hidden', '1', $hidden, 'Skrýt') ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/galerie">Zpět</a>
      <?= new \SubmitHelper($action == 'add' ? 'Přidat' : 'Upravit') ?>
    </div>
  </form>
</div>
