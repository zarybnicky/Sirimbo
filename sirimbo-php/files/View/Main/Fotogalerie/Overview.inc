<div class="container">
  <div class="row">
    <div class="col-4"><?= $sidemenu ?></div>
    <div class="col-8">
      <h2><?= $nadpis ?></h2>
      <?php foreach ($photos as $item): ?>
        <a href="<?= $item['href'] ?>" class="foto_preview no-a">
          <div class="foto_preview_in">
            <img alt="<?= $item['id'] ?>" src="<?= $item['src'] ?>" />
          </div>
        </a>
      <?php endforeach ?>
    </div>
  </div>
</div>
