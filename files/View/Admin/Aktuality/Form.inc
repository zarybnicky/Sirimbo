<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="name">Název</label>
      <input type="text" name="name" placeholder="Název"
             class="form-control" aria-describedby="nameHelp"
             value="<?= $this->name ?>" />
      <small id="nameHelp" class="form-text text-muted">Nejvíce 255 znaků</small>
    </div>
    <div class="form-group">
      <label for="category">Kategorie</label>
      <div class="form-check">
        <?= $this->radio('category', 1)->set($this->category == 1)->label('Články') ?>
      </div>
      <div class="form-check">
        <?= $this->radio('category', 3)->set($this->category == 3)->label('Krátké zprávy') ?>
      </div>
    </div>
    <div class="form-group">
      <label for="summary">Další info</label>
      <textarea class="form-control" name="summary" rows="5"><?= $this->summary ?></textarea>
    </div>
    <div class="form-group">
      <label for="text">Další info</label>
      <textarea class="form-control" name="text" rows="30"><?= $this->text ?></textarea>
    </div>
    <script src="/scripts/ckeditor/ckeditor.js"></script>
    <script>
     CKEDITOR.config.allowedContent = true;
     CKEDITOR.replace('text');
    </script>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
      <?= $this->submit('Přidat fotky')->data('action', 'foto') ?>
      <?= $this->submit(
          ($this->action == 'add') ? 'Přeskočit a uložit' : 'Upravit'
      )->data('action', 'save') ?>
    </div>
  </form>
</div>
