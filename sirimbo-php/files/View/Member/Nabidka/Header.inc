<div class="trenink-header">
  <div class="title"><?= $fullName ?></div>
  <div class="date"><?= $datum ?></div>

  <?php if ($hourMax > 0) : ?>
    <div>
      <span class="little">Maximálně hodin/pár:</span>
      <span class="nadpis"><?= $hourMax ?></span>
    </div>
  <?php endif ?>
  <div>
    <span class="little">Celkem hodin: </span>
    <span class="nadpis"><?= $hourTotal ?></span>
  </div>
  <div>
    <span class="little">Obsazených hodin: </span>
    <span class="nadpis"><?= $hourReserved ?></span>
  </div>
  <div>
    <span class="little">Volných hodin: </span>
    <span class="nadpis"><?= $hourFree ?></span>
  </div>

  <?php if ($canEdit) : ?>
    Admin:
    <a href="/admin/nabidka/edit/<?= $id ?>">obecné</a>,
    <a href="/admin/nabidka/detail/<?= $id ?>">tréninky</a>
  <?php endif ?>
</div>
