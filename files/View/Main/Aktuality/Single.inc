<?= $this->header($this->jmeno); ?>

<div class="container article">
  <div class="date little">
    <?php echo formatTimestamp($this->timestamp)?>
    <?php if ($this->canEdit) : ?>
      &nbsp;&nbsp;- <a href="/admin/aktuality/edit/<?php echo $this->id;?>">Upravit</a>
    <?php endif;?>
  </div>

  <div class="summary">
    <?php echo $this->description; ?>
  </div>

  <div class="image">
    <img alt="" src="<?php echo $this->title_photo_uri; ?>" />
  </div>

  <div class="text">
    <?php echo $this->text; ?>
  </div>

  <div class="fb-comments"
       data-href="http://tkolymp.cz/aktualne/<?php echo $this->id;?>"
       data-width="700">
  </div>
</div>
