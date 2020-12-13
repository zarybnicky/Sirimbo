<?php if (isset($this->notice)) : ?>
  <div class="container">
    <?= new NoticeHelper($this->notice) ?>
  </div>
<?php endif ?>
