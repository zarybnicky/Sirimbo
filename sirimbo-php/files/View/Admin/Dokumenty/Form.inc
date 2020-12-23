<div class="container">
  <form action="" method="POST">
    <b><?= $name ?></b><br><br>
    <div class="form-group">
      <?= (new \TextHelper('newname', ''))->placeholder('Nový název') ?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
      <?= (new \SubmitHelper('Upravit'))->data('action', 'edit_confirm') ?>
    </div>
  </form>
</div>
