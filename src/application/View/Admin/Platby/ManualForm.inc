<?php
echo $this->render('src/application/View/Admin/Platby/Navigation.inc');
echo '<h2>Ruční třídění plateb</span> (zbývá ', $this->remainingTotal, ')</h2>';
$form = array(
        array(
                'desc' => 'Datum:',
                'value' => (new \TKOlomouc\View\Helper\Date('date'))->setDate($this->guess['date'])->textBox(),
                'recogColumn' => $this->recognized['date']['column'],
                'recogValue' => $this->recognized['date']['value']
        ), array(
                'desc' => 'Částka (Kč):',
                'value' => '<input type="text" name="amount" value="' . $this->guess['amount'] . '"/>',
                'recogColumn' => $this->recognized['amount']['column'],
                'recogValue' => $this->recognized['amount']['value']
        ), array(
                'desc' => 'Uživatel: ',
                'value' => (new TKOlomouc\View\Helper\Select('variable'))->options($this->users)->value($this->guess['variable']),
                'recogColumn' => $this->recognized['variable']['column'],
                'recogValue' => $this->recognized['variable']['value']
        ), array(
                'desc' => 'Kategorie:',
                'value' => (new TKOlomouc\View\Helper\Select('specific'))->options($this->categories)->value($this->guess['specific']),
                'recogColumn' => $this->recognized['specific']['column'],
                'recogValue' => $this->recognized['specific']['value']
        ), array(
                'desc' => 'Prefix (rok):',
                'value' => '<input type="text" name="prefix" value="' . $this->guess['prefix'] . '"/>',
                'recogColumn' => '',
                'recogValue' => $this->recognized['prefix']['value']
        )
);
$formTable = (new TKOlomouc\View\Helper\Table())
    ->column('desc', '', 'r')
    ->column('value', 'Hodnota')
    ->column('recogColumn', 'Rozpoznáno:<br/>sloupec')
    ->column('recogValue', 'Rozpoznáno:<br/>hodnota')
    ->data($form);
?>
<form action="" method="post">
    <?php echo $formTable;?>
    <input type="hidden" name="id" value="<?php echo $this->id;?>" />
    <button name="action" value="confirm">Uložit</button>&nbsp;
    <button name="action" value="discard">Odstranit</button>&nbsp;
    <button name="action" value="skip">Přeskočit</button>
</form>
<br/>
<?php
echo '<h3>Data z banky</h3>';
echo (new TKOlomouc\View\Helper\Table())
    ->style('width:100%')
    ->showHeader(false)
    ->column('column', '', 'r', 'padding-right:5px')
    ->column('value', '', '', 'min-width:150px;padding-left:5px')
    ->data($this->raw);
?>