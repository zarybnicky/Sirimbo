<div class="container">
<h1><?php echo $this->jmeno;?></h1>
<?php echo formatTimestamp($this->timestamp)?>

<?php if ($this->canEdit) : ?>
&nbsp;&nbsp;- <a href="/admin/aktuality/edit/<?php echo $this->id;?>">Upravit</a>
<?php endif;?>
<br/>

<?php echo stripslashes(nl2br($this->text))?>
<br/><br/>
<div class="fb-comments"
     data-href="http://tkolymp.cz/aktualne/<?php echo $this->id;?>"
     data-width="710">
</div>
</div>
