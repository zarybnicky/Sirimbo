<?php echo $this->render('src/application/View/Admin/Platby/Navigation.inc');?>
<h2>Kategorie plateb</h2>
<h3>Odstranit specifický symbol?</h3>
<?php notice($this->redirect()->getMessage());?>

<form action="" method="POST">

Opravdu chcete odstranit specifický symbol "<?php echo $this->name;?>"?
<input type="hidden" name="data[]" value="<?php echo $this->id;?>" />
<?php echo getReturnInput();?>
<br/>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer();?>">Zpět</a>

</form>