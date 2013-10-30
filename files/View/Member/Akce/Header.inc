<div class="trenink_header" style="width:330px;">
    <div class="nadpis">
        <?php echo $this->jmeno;?>
    </div>
    <div class="nadpis">
        <?php echo $this->kde;?>
    </div>
    <div style="letter-spacing:1px;font-weight:bold;">
        <?php echo $this->datum;?>
    </div>
    <div>
        <span style="color:#572E00;font-size:115%;">Kapacita: </span>
        <?php echo $this->kapacita?>
        <br/>
        <span style="color:#572E00;font-size:115%;">Volných míst: </span>
        <?php echo $this->volno?>
        <br/>
    </div>
    <?php
    if ($this->canEdit) :
    ?>
    <span style="color:#572E00;font-size:115%;">Admin: </span>
    <a href="/admin/akce/edit/<?php echo $this->id;?>">obecné</a>
    <a href="/admin/akce/detail/<?php echo $this->id;?>">účastníci</a>
    <a href="/admin/akce/dokumenty/<?php echo $this->id;?>">dokumenty</a>
    <?php
    endif;
    if (isset($this->info)) :
    ?>
    <div style="text-align:left;">
        <span style="color:#572E00;font-size:115%;">Další informace: </span>
        <br/><?php echo $this->info;?>
    </div>
    <?php
    endif;
    if (isset($this->dokumenty) && $this->dokumenty != array()) :
    ?>
    <br/>
    <div style="text-align:left;">
    <span style="color:#572E00;font-size:115%;">Dokumenty: </span><br/>
    <?php
    foreach ($$this->dokumenty as $row):
    ?>
        <a href="/member/download?id=<?php echo $row['id'];?>">
            <?php echo $row['name'];?>
        </a>
    <?php
    endforeach;
    ?>
    </div>
    <?php
    endif;
    ?>
</div>