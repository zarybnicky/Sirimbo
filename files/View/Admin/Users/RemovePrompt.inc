<?php header_main('Správa uživatelů');?>

<form action="" method="POST">
Opravdu chcete odstranit uživatele:<br/><br/>
<?php foreach($this->data as $row): ?>
	<?php echo $row['login'], ' - ', $row['fullName'];?><br />
	<input type="hidden" name="users[]" value="<?php echo $row['id']?>" />
<?php endforeach;?>
<br/>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo $this->returnURL;?>">Zpět</a>
</form>