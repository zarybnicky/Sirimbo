<?= $this->header($this->nadpis) ?>
<?php if (isset($this->notice)): ?>
  <div class="container">
    <?= $this->notice($this->notice) ?>
  </div>
<?php endif ?>
