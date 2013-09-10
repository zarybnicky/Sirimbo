<?php
$mainForm = (new TableHelper())->table()
	->column('desc', '', 'r')
	->column('value', 'Hodnota')
	->data(array(
		array(
				'desc' => 'Typ:',
				'value' => '<label>' . getRadio('type', '0', true) . 'Běžné platby</label><br/>' .
					'<label>' . getRadio('type', '1') . 'Členské příspěvky</label>'
		), array(
				'desc' => 'Jméno:',
				'value' => '<input type="text" name="name" value="' . post('name') . '"/>'
		), array(
				'desc' => 'Popis: ',
				'value' => '<textarea rows="10" cols="50" name="description">' . post('description') . '</textarea>'
		), array(
				'desc' => 'Násobitel částky: ',
				'value' => '<input type="text" name="base" value="' . post('base') . '"/>'
		), array(
				'desc' => 'Přiřazené skupiny: ',
				'value' => $this->skupinySelect
		)
	));
if(!empty($this->categories)) {
	$categoryForm = (new TableHelper())->table()
		->column('buttons', '')
		->column('name', 'Jméno')
		->column('specific', 'Specifický<br/>symbol')
		->column('amount', 'Očekávaná<br/>částka')
		->column('dueDate', 'Splatnost')
		->column('validDate', 'Platnost')
		->column('usePrefix', 'Používá<br/>prefix roku')
		->column('useBase', 'Používá<br/>násobitel částky')
		->column('archive', 'Je neaktivní')
		->data($this->categories);
}
$symbolSelect = (new SelectHelper())->select('symbol')
	->options($this->symbols);
?>

<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php header_main('Kategorie plateb');?>
<?php header_minor($this->action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii');?>
<form action="" method="post">
	<?php echo $mainForm;?>
	<button name="action" value="confirm">Uložit</button>&nbsp;<a href="/admin/platby/category">Zpět</a><br/><br/>
</form>
<?php if($this->action != 'add'): ?>
	<?php header_minor('Specifické symboly');?>
	<?php if(isset($categoryForm)) echo $categoryForm;?>
	<span class="big">Přidat:</span>
	<a href="/admin/platby/category/add_category?group=<?php echo $this->id;?>">nový specifický symbol</a>
	nebo existující: <?php echo $symbolSelect;?> <button type="submit" name="action" value="add_symbol">odeslat</button>
<?php endif;?>