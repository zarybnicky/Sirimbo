<div class="trenink_header">
    <div class="nadpis"><?php echo $this->fullName;?></div>
    <div style="letter-spacing:1px;font-weight:bold;"><?php echo $this->datum;?></div>

    <?php if ($this->canEdit) : ?>
    <span style="color:#572E00;font-size:115%;">Admin: </span>
    <a href="/admin/nabidka/edit/<?php echo $this->id;?>">obecné</a>,
    <a href="/admin/nabidka/detail/<?php echo $this->id;?>">tréninky</a>
    <?php endif;?>

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
</div>