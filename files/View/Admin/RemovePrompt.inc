<h1><?php echo $this->header;?></h1>
<form action="" method="POST">
<?php echo $this->prompt;?><br/><br/>
<?php foreach($this->data as $row): ?>
	<?php echo $row['text']?>
	<input type="hidden" name="data[]" value="<?php echo $row['id']?>" />
	<br />
<?php endforeach;?>
<br/>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo $this->returnURL;?>">Zpět</a>
</form>