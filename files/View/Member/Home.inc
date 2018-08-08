<?= $this->header('Novinky') ?>

<div class="container">
  <?= $this->partnerRequest()->getAll(); ?>

  <?php
  if (!$this->data) {
    echo $this->notice('Žádné novinky');
    return;
  }
  ?>

  <?php foreach ($this->data as $item): ?>
    <div class="no_item">
      <div class="no_text"><?= $item['text'];?></div>
      <div class="no_date" style="text-align:right;">
        <?= $item['user'], ', ', $item['timestamp'];?>
        <?php if ($this->canEdit): ?>
          <a href="/admin/novinky/remove?id=<?= $item['id'];?>">Odstranit</a>
        <?php endif;?>
      </div>
    </div><hr/>
  <?php endforeach;?>
</div>
