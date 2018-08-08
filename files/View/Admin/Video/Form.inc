<?= $this->header('Správa videí', $this->header) ?>

<div class="container">
  <form action="" method="post">
    <div class="form-group">
      <label for="name">Jméno</label>
      <?= $this->text('name', $this->name) ?>
    </div>
    <div class="form-group">
      <label for="text">Jméno</label>
      <textarea name="text" class="form-control" rows="5"><?= $this->name ?>"</textarea>
    </div>
    <div class="form-group">
      <label for="name">URI</label>
      <?= $this->text('uri', $this->uri) ?>
    </div>
    <div class="form-group">
      <?= $this->submit('Upravit') ?>
      <a class="btn btn-outline-primary" href="/admin/video">Zpět</a>
    </div>
  </form>
</div>
