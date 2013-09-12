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
		)
	));
if(!empty($this->category)) {
	$categoryForm = (new TableHelper())->table()
		->column('buttons', '', '', 'width:50px')
		->column('name', 'Jméno')
		->column('specific', 'Specifický<br/>symbol')
		->column('amount', 'Očekávaná<br/>částka')
		->column('dueDate', 'Splatnost')
		->column('validDate', 'Platnost')
		->column('usePrefix', 'Používá<br/>prefix roku')
		->column('useBase', 'Používá<br/>násobitel částky')
		->column('archive', 'Je neaktivní')
		->data($this->category);
}
$categorySelect = (new SelectHelper())->select('category')
	->options($this->categorySelect);

if(!empty($this->skupiny)) {
	$skupinyForm = (new TableHelper())->table()
		->column('buttons', '', '', 'width:50px')
		->column('name', 'Jméno')
		->data($this->skupiny);
}
$skupinySelect = (new SelectHelper())->select('skupiny')
	->options($this->skupinySelect);
?>

<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php header_main('Kategorie plateb');?>
<?php header_minor($this->action == 'add' ? 'Přidat kategorii' : 'Upravit kategorii');?>
<form action="" method="post">
	<?php echo $mainForm;?>
	<button name="action" value="confirm">Uložit</button>&nbsp;<a href="/admin/platby/category">Zpět</a><br/><br/>
</form>
<?php if($this->action != 'add'): ?>
	<form action="" method="post">
	<?php header_minor('Specifické symboly');?>
	<?php if(isset($categoryForm)) echo $categoryForm;?>
	<span class="big">Přidat:</span>
	<a href="/admin/platby/category/add_category?group=<?php echo $this->id;?>">nový specifický symbol</a>
	nebo existující: <?php echo $categorySelect;?>
		<button type="submit" name="action" value="category">odeslat</button>
	</form><br/>
	
	<form action="" method="post">
	<?php header_minor('Skupiny');?>
	<?php if(isset($skupinyForm)) echo $skupinyForm;?>
	<span class="big">Přidat:</span>
	<a href="/admin/skupiny/add?group=<?php echo $this->id;?>">novou skupinu</a>
	nebo existující: <?php echo $skupinySelect;?>
		<button type="submit" name="action" value="skupiny">odeslat</button>
	</form>
<?php endif;?>