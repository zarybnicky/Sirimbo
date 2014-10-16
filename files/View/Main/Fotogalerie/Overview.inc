<div class="container">
  <div class="container-fifth">
    <?php echo $this->sidebar ?>
  </div>
  <div class="container-four-fifths">
    <h2><?php echo $this->nadpis;?></h2>
    <?php foreach ($this->photos as $item): ?>
      <a href="<?php echo $item['href'];?>" class="foto_preview no-a">
        <div class="foto_preview_in">
          <img alt="<?php echo $item['id'];?>" src="<?php echo $item['src'];?>" />
        </div>
      </a>
    <?php endforeach;?>
  </div>
</div>
