<div class="container">
  <?php foreach ($data as $item): ?>
    <div>
      <div class="up_header">
        <div style="float:left;">
          <span class="nadpis"><?= $item['nadpis'] ?></span>
          <?php if ($item['canEdit']): ?>
            &nbsp;-&nbsp;<a href="/admin/nastenka/edit/<?= $item['id'] ?>">Editovat</a>
          <?php endif ?>
          <?php if ($item['skupinyBoxes']): ?>
          <div style="padding:2px 0;"><div style="float:left;" class="little">skupiny:&nbsp;</div>
            <?= $item['skupinyBoxes'] ?>
          </div>
          <?php endif ?>
        </div>
        <div style="float:right;text-align:right;">
          <div>
            <span class="little">přidal: </span><?= $item['addedBy'] ?>
            <br/>
            <span class="little">přidáno: </span><?= $item['addedTimestamp'] ?>
          </div>
        </div>
        <div style="clear:both;"></div>
      </div>
      <div style="padding-top:8px;">
        <?= $item['text'] ?>
      </div>
    </div>
    <hr/>
  <?php endforeach ?>
  <div style="text-align:center;"><?= $navigation ?></div>
</div>
