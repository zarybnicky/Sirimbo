<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php header_main('Kategorie plateb');?>
<?php header_minor('Odstranit specifický symbol?');?>
<?php notice($this->redirect()->getMessage());?>

<form action="" method="POST">

Opravdu chcete odstranit specifický symbol "<?php echo $this->name;?>"?
<input type="hidden" name="data[]" value="<?php echo $this->id;?>" />
<br/>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="/admin/platby/structure">Zpět</a>

</form>