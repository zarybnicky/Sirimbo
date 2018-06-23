<?= $this->header($this->jmeno); ?>

<div class="container full article">
  <div class="date little">
    <?= formatTimestamp($this->timestamp)?>
    <?php if ($this->canEdit) : ?>
      &nbsp;&nbsp;- <a href="/admin/aktuality/edit/<?= $this->id;?>">Upravit</a>
    <?php endif;?>
  </div>

  <div class="summary"><?= $this->description; ?></div>

  <div class="image">
    <img alt="" src="<?= $this->title_photo_uri; ?>" />
  </div>

  <div class="text"><?= $this->text; ?></div>

  <div class="fb-comments"
       data-href="http://tkolymp.cz/aktualne/<?= $this->id;?>"
       data-width="700">
  </div>
</div>
