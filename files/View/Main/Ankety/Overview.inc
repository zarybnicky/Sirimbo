<h1>Ankety</h1>

<?php foreach ($this->data as $row): ?>
    <b><?php echo $row['text'];?></b>
    <?php if ($row['canEdit']): ?>
        &nbsp;-&nbsp;<a href="/admin/ankety/edit/<?php echo $row['id'];?>">Editovat</a>
    <?php endif;?>
    <br/>
    <?php foreach ($row['items'] as $item): ?>
        <div style="width:60%;position:relative;">
            <?php echo $item['text'];?>
            <div style="float:right;"><?php echo $item['pocet'];?></div>
        </div>
        <div style="width:60%;height:10px;line-height:10px;background-color:#D5D5D5;">
            <div style="width:<?php echo $item['width'];?>%;height:10px;background-color:#<?php echo $item['color'];?>;"></div>
        </div>
        <div style="height:7px;">&nbsp;</div>
    <?php endforeach;?>
    <br />
<?php endforeach;?>