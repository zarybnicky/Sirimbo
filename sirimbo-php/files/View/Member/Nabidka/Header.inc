<div class="trenink-header">
  <div class="title"><?= $this->fullName ?></div>
  <div class="date"><?= $this->datum ?></div>

  <?php if ($this->hourMax > 0) : ?>
    <div>
      <span class="little">Maximálně hodin/pár:</span>
      <span class="nadpis"><?= $this->hourMax ?></span>
    </div>
  <?php endif ?>
  <div>
    <span class="little">Celkem hodin: </span>
    <span class="nadpis"><?= $this->hourTotal ?></span>
  </div>
  <div>
    <span class="little">Obsazených hodin: </span>
    <span class="nadpis"><?= $this->hourReserved ?></span>
  </div>
  <div>
    <span class="little">Volných hodin: </span>
    <span class="nadpis"><?= $this->hourFree ?></span>
  </div>

  <?php if ($this->canEdit) : ?>
    Admin:
    <a href="/admin/nabidka/edit/<?= $this->id ?>">obecné</a>,
    <a href="/admin/nabidka/detail/<?= $this->id ?>">tréninky</a>
  <?php endif ?>
</div>
