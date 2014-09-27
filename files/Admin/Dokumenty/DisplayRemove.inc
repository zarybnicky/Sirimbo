<h1>Správa dokumentů</h1>
<?php
echo $this->notice($this->redirect()->getMessage());

if (!is_array(get('u')))
    echo $this->notice('Žádné dokumenty');
?>
<form action="" method="POST">
Opravdu chcete odstranit dokumenty:<br/><br/>
<?php
foreach (get('u') as $id) {
    $name = DBDokumenty::getDokumentName($id);
    echo $name, '<br />';
    echo '<input type="hidden" name="dokumenty[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>
