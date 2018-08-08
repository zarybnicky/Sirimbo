<?= $this->header('Správa dokumentů') ?>

<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="staticEmail">Starý název</label>
      <input type="text" readonly class="form-control-plaintext" id="staticEmail" value="<?= $this->name ?>">
    </div>
    <div class="form-group">
      <label for="name">Nový název</label>
      <?= $this->text('newname')->placeholder('Nový název') ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
      <?= $this->submit('Upravit')->data('action', 'edit_confirm') ?>
    </div>
  </form>
</div>
