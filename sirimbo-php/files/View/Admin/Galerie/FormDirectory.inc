<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Název</label>
      <?= \Utils::text('name', $name) ?>
    </div>
    <div class="form-group">
      <label for="parent">Nadsložka</label>
      <?= \Utils::selectAssoc('parent', ['id' => '0', 'text' => '---'] + $dirs, 'id', 'text', $parent) ?>
    </div>
    <div class="form-check">
      <?= \Utils::checkbox('hidden', '1', $hidden, 'Skrýt') ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/galerie">Zpět</a>
      <button class="btn btn-primary"><?= $action ?></button>
    </div>
  </form>
</div>
