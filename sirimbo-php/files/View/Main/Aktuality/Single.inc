<div class="container article">
  <small>
    <?= \Format::timestamp($timestamp)?>
    <?php if ($canEdit) : ?>
      &nbsp;&nbsp;- <a href="/admin/aktuality/edit/<?= $id ?>">Upravit</a>
    <?php endif ?>
  </small><br>

  <h3><?= $description ?></h3>

  <div class="row">
    <div class="col-md-10 col-lg-7 text-justify">
      <img style="display:block;max-width:100%" alt="" src="<?= $title_photo_uri ?>" />

      <?= $text ?>

      <div class="fb-comments" data-href="http://tkolymp.cz/aktualne/<?= $id ?>"></div>
    </div>
  </div>
</div>
