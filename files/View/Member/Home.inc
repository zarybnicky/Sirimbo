<?= $this->header('Novinky') ?>

<div class="container full">
  <?php echo $this->notice($this->redirect()->getMessage());?>
  <?php echo $this->partnerRequest()->getAll(); ?>

  <?php
  if (!$this->data) {
    echo $this->notice('Žádné novinky');
    return;
  }
  ?>

  <?php foreach ($this->data as $item): ?>
    <div class="no_item">
      <div class="no_text"><?php echo $item['text'];?></div>
      <div class="no_date" style="text-align:right;">
        <?php echo $item['user'], ', ', $item['timestamp'];?>
        <?php if ($this->canEdit): ?>
          <a href="/admin/novinky/remove?id=<?php echo $item['id'];?>">Odstranit</a>
        <?php endif;?>
      </div>
    </div><hr/>
  <?php endforeach;?>
</div>
