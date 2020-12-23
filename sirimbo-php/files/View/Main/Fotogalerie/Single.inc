<div class="container">
  <div class="row">
    <div class="col-4"><?= $sidemenu ?></div>
    <div class="col-8">
      <div style="text-align:center;">
        <?php if ($hasPrev): ?>
          <a href="<?= $prevURI ?>">&lt;&lt;</a>
        <?php else: ?>
          &lt;&lt;
        <?php endif ?>

        &nbsp;&bull;&nbsp;
        <a href="<?= $returnURI ?>">Zpět</a>
        &nbsp;&bull;&nbsp;

        <?php if ($hasNext): ?>
          <a href="<?= $nextURI ?>">&gt;&gt;</a>
        <?php else: ?>
          &gt;&gt;
        <?php endif ?>
      </div>

      <a href="<?= $returnURI ?>">
        <div class="foto_full">
          <img alt="<?= $id ?>" src="<?= $src ?>" style="max-width:100%"/>
        </div>
      </a>
    </div>
  </div>
</div>
