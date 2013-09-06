<?php
echo $this->render('files/View/Admin/Platby/OverviewNavigation.inc');
echo '<br/><span style="font-size:115%;color:#572E00;">Ruční třídění plateb</span> (zbývá ', $this->remainingTotal, ')<br/><br/>';
$form = array(
		array(
				'desc' => 'Datum:',
				'value' => $this->date('date')->setDate($this->guess['date'])->textBox(),
				'recogColumn' => $this->recognized['date']['column'],
				'recogValue' => $this->recognized['date']['value']
		), array(
				'desc' => 'Částka (Kč):',
				'value' => '<input type="text" name="amount" value="' . $this->guess['amount'] . '"/>',
				'recogColumn' => $this->recognized['amount']['column'],
				'recogValue' => $this->recognized['amount']['value']
		), array(
				'desc' => 'Uživatel: ',
				'value' => (new SelectHelper())->select('variable')->options($this->users)->value($this->guess['variable']),
				'recogColumn' => $this->recognized['variable']['column'],
				'recogValue' => $this->recognized['variable']['value']
		), array(
				'desc' => 'Kategorie:',
				'value' => (new SelectHelper())->select('specific')->options($this->categories)->value($this->guess['specific']),
				'recogColumn' => $this->recognized['specific']['column'],
				'recogValue' => $this->recognized['specific']['value']
		)
);
$formTable = $this->table()
	->column('desc', '', 'r')
	->column('value', 'Hodnota')
	->column('recogColumn', 'Rozpoznáno:<br/>sloupec')
	->column('recogValue', 'Rozpoznáno:<br/>hodnota')
	->data($form);
?>
<form action="" method="post">
	<?php echo $formTable;?>
	<input type="hidden" name="id" value="<?php echo $this->id;?>" />
	<button name="action" value="confirm">Uložit</button>&nbsp;<button name="action" value="discard">Odstranit</button>
</form>
<br/>
<?php
header_minor('Data z banky:');
echo $this->table()
	->style('width:100%')
	->showHeader(false)
	->column('column', '', 'r', 'padding-right:5px')
	->column('value', '', '', 'min-width:150px;padding-left:5px')
	->data($this->raw);
?>