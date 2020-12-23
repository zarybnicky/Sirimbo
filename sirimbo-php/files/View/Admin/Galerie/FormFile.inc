<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Popis</label>
      <?= \Utils::text('name', $name) ?>
    </div>
    <div class="form-group">
      <label for="parent">Nadsložka</label>
      <?= \Utils::selectAssoc('parent', ['id' => '0', 'text' => '---'] + $dirs, 'id', 'text', $parent) ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="<?= $returnURI ?>">Zpět</a>
      <?= \Utils::submit('Upravit') ?>
    </div>
  </form>
</div>
