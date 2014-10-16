<div class="container article-preview">
  <h1><?php echo $this->nadpis;?></h1>
  <?php foreach ($this->data as $key => $row) : ?>
    <a href="/aktualne/<?php echo $row['id'];?>" class="big">
      <?php echo $row['jmeno'];?>
    </a>

    <span class="date little">
      <?php echo formatTimestamp($row['timestamp'])?>
      <?php if ($row['canEdit']) : ?>
        - <a href="/admin/aktuality/edit/<?php echo $row['id'];?>">Upravit</a>
      <?php endif;?>
    </span>

    <div class="preview">
      <?php if ($row['preview']): ?>
        <?php echo stripslashes(nl2br($row['preview']));?><br/>
        <a href="/aktualne/<?php $row['id']?>" class="little">více zde &raquo;</a><br/>
      <?php endif; ?>
      <br/>
    </div>
  <?php endforeach;?>
</div>
