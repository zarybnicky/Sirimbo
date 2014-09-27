<div class="container">
<div style="text-align:center;">

<?php if ($this->hasPrev): ?>
    <a href="<?php echo $this->prevURI;?>">&lt;&lt;</a>
<?php else: ?>
    &lt;&lt;
<?php endif;?>

&nbsp;&bull;&nbsp;
<a href="<?php echo $returnURI;?>">Zpět</a>
&nbsp;&bull;&nbsp;

<?php if ($this->hasNext): ?>
    <a href="<?php echo $this->nextURI;?>">&gt;&gt;</a>
<?php else: ?>
    &gt;&gt;
<?php endif;?>

</div>

<a href="<?php echo $this->returnURI;?>">
    <div class="f_full"><div class="f_img">
        <img alt="<?php echo $this->id;?>" src="<?php echo $this->src;?>" />
    </div></div>
</a>
</div>
