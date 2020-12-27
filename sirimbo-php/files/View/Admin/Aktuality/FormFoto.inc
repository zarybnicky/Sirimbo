<div class="container">
  <form class="form-inline" action="" method="get">
    <div class="form-group">
      <?= \Utils::select('dir', ['none' => '------ vyberte složku ------'] + $dirs, $dir ?: 'none') ?>
    </div>
    <div class="form-group"><button class="btn btn-primary">Zobrazit</button></div>
  </form>

  <form action="" method="post">
    <?php foreach ($photos as $item): ?>
      <div class="foto_preview">
        <div class="foto_preview_in">
          <img alt="<?= $item['id'] ?>" src="<?= $item['src'] ?>" />
        </div>
        <div class="foto_desc">
          <?= \Utils::radio('foto', $item['id'], $item['id'] == $checked, $item['name']) ?>
          <br/><br/>
        </div>
      </div>
    <?php endforeach ?>
    <div class="form-group" style="clear:both">
      <button class="btn btn-primary">Potvrdit</button>
      <a class="btn btn-outline-primary" href="/admin/aktuality">Zpět</a>
    </div>
  </form>
</div>
