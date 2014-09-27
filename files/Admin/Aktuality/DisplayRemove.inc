<h1>Správa aktualit</h1>
<?php
echo $this->notice($this->redirect()->getMessage());

if (!is_array(get('u')))
    echo $this->notice('Žádné aktuality');
?>
<form action="" method="POST">
Opravdu chcete odstranit články:<br/><br/>
<?php
foreach (get('u') as $id) {
    $data = DBAktuality::getSingleAktualita($id);
    echo $data['at_jmeno'], '<br />';
    echo '<input type="hidden" name=aktuality[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>
