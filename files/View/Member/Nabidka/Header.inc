<div class="trenink-header">
  <div class="title"><?php echo $this->fullName;?></div>
  <div class="date"><?php echo $this->datum;?></div>

  <?php if ($this->hourMax > 0) : ?>
    <div>
      <span class="little">Maximálně hodin/pár:</span>
      <span class="nadpis"><?php echo $this->hourMax;?></span>
    </div>
  <?php endif;?>
  <div>
    <span class="little">Celkem hodin: </span>
    <span class="nadpis"><?php echo $this->hourTotal;?></span>
  </div>
  <div>
    <span class="little">Obsazených hodin: </span>
    <span class="nadpis"><?php echo $this->hourReserved;?></span>
  </div>
  <div>
    <span class="little">Volných hodin: </span>
    <span class="nadpis"><?php echo $this->hourFree;?></span>
  </div>

  <?php if ($this->canEdit) : ?>
    Admin:
    <a href="/admin/nabidka/edit/<?php echo $this->id;?>">obecné</a>,
    <a href="/admin/nabidka/detail/<?php echo $this->id;?>">tréninky</a>
  <?php endif;?>
</div>
