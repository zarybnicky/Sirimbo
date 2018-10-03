<?= $this->header($this->jmeno) ?>

<div class="container article">
  <small>
    <?= formatTimestamp($this->timestamp)?>
    <?php if ($this->canEdit) : ?>
      &nbsp;&nbsp;- <a href="/admin/aktuality/edit/<?= $this->id ?>">Upravit</a>
    <?php endif ?>
  </small><br>

  <h3><?= $this->description ?></h3>

  <div class="row">
    <div class="col-md-9 col-lg-6 text-justify">
      <img style="display:block;max-width:100%" alt="" src="<?= $this->title_photo_uri ?>" />

      <?= $this->text ?>

      <div class="fb-comments" data-href="http://tkolymp.cz/aktualne/<?= $this->id ?>"></div>
    </div>
  </div>
</div>
