<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
header_main('Jednotlivé platby');

if($this->action == 'add') header_minor('Přidat platbu');
else header_minor('Upravit platbu');

$form = array(
		array(
				'desc' => 'Datum:',
				'value' => $this->date('date')->textBox()
		), array(
				'desc' => 'Částka (Kč):',
				'value' => '<input type="text" name="amount" value="' . post('amount') . '"/>'
		), array(
				'desc' => 'Uživatel: ',
				'value' => (new SelectHelper())->select('variable')->options($this->users)
		), array(
				'desc' => 'Specifický symbol:',
				'value' => (new SelectHelper())->select('specific')->options($this->categories)
		), array(
				'desc' => 'Prefix (rok):',
				'value' => '<input type="text" name="prefix" value="' . post('prefix') . '"/>'
		)
);
$formTable = $this->table()
	->column('desc', '', 'r')
	->column('value', 'Hodnota')
	->data($form);
?>
<form action="" method="post">
	<?php echo $formTable;?>
	<button name="action" value="confirm">Uložit</button>&nbsp;<a href="/admin/platby/items">Zpět</a>
</form>
<br/>
<?php
if(!empty($this->raw)) {
	header_minor('Data z banky:');
	echo $this->table()
		->style('width:100%')
		->showHeader(false)
		->column('column', '', 'r', 'padding-right:5px')
		->column('value', '', '', 'min-width:150px;padding-left:5px')
		->data($this->raw);
}
?>