<?= $this->header('Správa dokumentů') ?>

<div class="container">
  <form action="" method="POST">
    <b><?= $this->name ?></b><br><br>
    <div class="form-group">
      <?= $this->text('newname', '')->placeholder('Nový název') ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
      <?= $this->submit('Upravit')->data('action', 'edit_confirm') ?>
    </div>
  </form>
</div>
