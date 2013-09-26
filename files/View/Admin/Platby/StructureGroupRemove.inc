<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php header_main('Kategorie plateb');?>
<?php header_minor('Odstranit kategorii?');?>
<?php notice($this->redirect()->getMessage());?>

<form action="" method="POST">

Opravdu chcete odstranit kategorii plateb "<?php echo $this->name;?>"?
<input type="hidden" name="data[]" value="<?php echo $this->id;?>" />
<?php echo getReturnInput();?>
<br/>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer();?>">Zpět</a>

</form>