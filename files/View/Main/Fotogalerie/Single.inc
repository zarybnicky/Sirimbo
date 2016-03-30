<div class="container full">
  <div class="container fifth">
    <?php echo $this->sidemenu ?>
  </div>
  <div class="container four-fifths">
    <div style="text-align:center;">
      <?php if ($this->hasPrev): ?>
        <a href="<?php echo $this->prevURI;?>">&lt;&lt;</a>
      <?php else: ?>
        &lt;&lt;
      <?php endif;?>

      &nbsp;&bull;&nbsp;
      <a href="<?php echo $this->returnURI;?>">Zpět</a>
      &nbsp;&bull;&nbsp;

      <?php if ($this->hasNext): ?>
        <a href="<?php echo $this->nextURI;?>">&gt;&gt;</a>
      <?php else: ?>
        &gt;&gt;
      <?php endif;?>
    </div>

    <a href="<?php echo $this->returnURI;?>">
      <div class="foto_full">
        <img alt="<?php echo $this->id;?>" src="<?php echo $this->src;?>" style="max-width:100%"/>
      </div>
    </a>
  </div>
</div>
