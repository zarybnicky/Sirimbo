<h2><?php echo $this->nadpis;?></h2>
<?php foreach ($this->photos as $item): ?>
    <a href="<?php echo $item['href'];?>" class="f_preview">
        <div class="f_img">
            <img alt="<?php echo $item['id'];?>" src="<?php echo $item['src'];?>" />
        </div>
    </a>
<?php endforeach;?>