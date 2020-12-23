<div class="container">
  <form class="form-inline" action="" method="get">
    <div class="form-group">
      <?= new \SelectHelper('dir', ['none' => '------ vyberte složku ------'] + $dirs, $dir ?: 'none') ?>
    </div>
    <div class="form-group"><?= new \SubmitHelper('Zobrazit') ?></div>
  </form>

  <form action="" method="post">
    <?php foreach ($photos as $item): ?>
      <div class="foto_preview">
        <div class="foto_preview_in">
          <img alt="<?= $item['id'] ?>" src="<?= $item['src'] ?>" />
        </div>
        <div class="foto_desc">
          <?= (new \RadioHelper('foto', $item['id'], $item['id'] == $checked))->label($item['name']) ?>
          <br/><br/>
        </div>
      </div>
    <?php endforeach ?>
    <div class="form-group" style="clear:both">
      <?= new \SubmitHelper('Potvrdit') ?>
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
    </div>
  </form>
</div>
