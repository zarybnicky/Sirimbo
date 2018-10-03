<div class="container">
  <div class="row">
    <div class="col-4"><?= $this->sidemenu ?></div>
    <div class="col-8">
      <div style="text-align:center;">
        <?php if ($this->hasPrev): ?>
          <a href="<?= $this->prevURI ?>">&lt;&lt;</a>
        <?php else: ?>
          &lt;&lt;
        <?php endif ?>

        &nbsp;&bull;&nbsp;
        <a href="<?= $this->returnURI ?>">Zpět</a>
        &nbsp;&bull;&nbsp;

        <?php if ($this->hasNext): ?>
          <a href="<?= $this->nextURI ?>">&gt;&gt;</a>
        <?php else: ?>
          &gt;&gt;
        <?php endif ?>
      </div>

      <a href="<?= $this->returnURI ?>">
        <div class="foto_full">
          <img alt="<?= $this->id ?>" src="<?= $this->src ?>" style="max-width:100%"/>
        </div>
      </a>
    </div>
  </div>
</div>
