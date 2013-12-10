<h1><?php echo $this->nadpis;?></h1>
<?php foreach ($this->data as $key => $row) : ?>
    <a href="/aktualne/<?php echo $row['id'];?>" style="font-size:115%;color:#572E00;margin:5px 0 5px 0;">
        <?php echo $row['jmeno'];?>
    </a>
    <br/>

    <?php echo formatTimestamp($row['timestamp'])?>
    <?php if ($row['canEdit']) : ?>
        &nbsp;&nbsp;- <a href="/admin/aktuality/edit/<?php echo $row['id'];?>">Upravit</a>
    <?php endif;?>
    <br/>

    <?php echo stripslashes(nl2br($row['preview']));?>
    <a href="/aktualne/<?php $row['id']?>">...</a><br/>

    <?php if (count($this->data)-1 != $key) : ?>
        <hr/>
    <?php endif;?>
<?php
endforeach;