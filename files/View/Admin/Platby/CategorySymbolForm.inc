<?php
$mainForm = (new TableHelper())->table()
->column('desc', '', 'r')
->column('value', 'Hodnota')
->data(array(
		array(
				'desc' => 'Jméno:',
				'value' => '<input type="text" name="name" value="' . post('name') . '"/>'
		), array(
				'desc' => 'Specifický symbol:',
				'value' => '<input type="text" name="symbol" value="' . post('symbol') . '"/>'
		), array(
				'desc' => 'Očekávaná částka:',
				'value' => '<input type="text" name="amount" value="' . post('amount') . '"/>'
		), array(
				'desc' => 'Splatnost:',
				'value' => (new DateHelper())->date('dueDate')
		), array(
				'desc' => 'Platnost:',
				'value' => (new DateHelper())->date('validRange')->range()
		), array(
				'desc' => 'Povolit prefix roku?:',
				'value' => getCheckbox('usePrefix', '1')
		), array(
				'desc' => 'Archivovat?:',
				'value' => getCheckbox('archive', '1')
		)
));
?>
<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php header_main('Kategorie plateb');?>
<?php header_minor($this->action == 'add' ? 'Přidat specifický symbol' : 'Upravit specifický symbol');?>
<form action="" method="post">
	<?php echo $mainForm;?>
	<button name="action" value="confirm">Uložit</button>&nbsp;<a href="/admin/platby/category">Zpět</a><br/><br/>
</form>
<?php if($this->action == 'TODOd'): ?>
	<?php header_minor('Specifické symboly');?>
	<?php if(isset($categoryForm)) echo $categoryForm;?>
	<span class="big">Přidat:</span>
	<a href="/admin/platby/category/add_category?group=<?php echo $this->id;?>">nový specifický symbol</a>
	nebo existující: <?php echo $symbolSelect;?> <button type="submit" name="action" value="add_symbol">odeslat</button>
<?php endif;?>