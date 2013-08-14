<div style="text-align:center;">

<?php if($this->hasPrev): ?>
	<a href="<?php echo $this->prevURL;?>">&lt;&lt;</a>
<?php else: ?>
	&lt;&lt;
<?php endif;?>

&nbsp;&bull;&nbsp;
<a href="<?php echo $returnURL;?>">Zpět</a>
&nbsp;&bull;&nbsp;

<?php if($this->hasNext): ?>
	<a href="<?php echo $this->nextURL;?>">&gt;&gt;</a>
<?php else: ?>
	&gt;&gt;
<?php endif;?>

</div>

<a href="<?php echo $this->returnURL;?>">
	<div class="f_full"><div class="f_img">
		<img alt="<?php echo $this->id;?>" src="<?php echo $this->src;?>" />
	</div></div>
</a>