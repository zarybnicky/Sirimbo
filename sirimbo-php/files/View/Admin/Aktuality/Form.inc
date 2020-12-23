<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="name">Název</label>
      <input type="text" name="name" placeholder="Název"
             class="form-control" aria-describedby="nameHelp"
             value="<?= $name ?>" />
      <small id="nameHelp" class="form-text text-muted">Nejvíce 255 znaků</small>
    </div>
    <?php if (isset($createdAt)) : ?>
      <div class="form-group">
        <label for="createdAt">Publikováno</label>
        <?= \Utils::text('createdAt', $createdAt) ?>
      </div>
    <?php endif ?>
    <div class="form-group">
      <label for="summary">Shrnutí</label>
      <textarea class="form-control" name="summary" rows="5"><?= $summary ?></textarea>
    </div>
    <div class="form-group">
      <label for="text">Text článku</label>
      <textarea class="form-control" name="text" rows="30"><?= $text ?></textarea>
    </div>
    <script src="/scripts/ckeditor/ckeditor.js"></script>
    <script>
     CKEDITOR.config.allowedContent = true;
     CKEDITOR.replace('text');
    </script>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
      <?= \Utils::submit('Přidat fotky', 'action', 'foto') ?>
      <?= \Utils::submit(($action == 'add') ? 'Přeskočit a uložit' : 'Upravit', 'action', 'save') ?>
    </div>
  </form>
</div>
