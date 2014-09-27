<div class="container">
<h1>Upozornění</h1>
<?php foreach ($this->data as $item): ?>
<div>
    <div class="up_header">
        <div style="float:left;">
            <span class="nadpis"><?php echo $item['nadpis'];?></span>
            <?php if ($item['canEdit']): ?>
                &nbsp;-&nbsp;<a href="/admin/nastenka/edit/<?php echo $item['id'];?>">Editovat</a>
            <?php endif;?>
            <div style="padding:2px 0;"><div style="float:left;" class="little">skupiny:&nbsp;</div>
            <?php foreach ($item['skupinyBoxes'] as $box) echo $box; ?>
            </div>
        </div>
        <div style="float:right;text-align:right;">
            <div>
                <span class="little">přidal: </span><?php echo $item['addedBy'];?>
                <br/>
                <span class="little">přidáno: </span><?php echo $item['addedTimestamp'];?>
            </div>
        </div>
        <div style="clear:both;"></div>
    </div>
    <div style="padding-top:8px;">
        <?php echo $item['text'];?>
    </div>
</div>
<hr/>
<?php endforeach;?>
<div style="text-align:center;"><?php echo $this->navigation;?></div>
</div>
