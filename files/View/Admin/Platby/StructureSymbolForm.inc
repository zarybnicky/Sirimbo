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
if(!empty($this->groups)) {
	$groupForm = (new TableHelper())->table()
		->column('buttons', '', '', 'width:75px')
		->column('type', 'Typ')
		->column('name', 'Jméno')
		->column('base', 'Násobitel částky')
		->data($this->groups);
}
if(!empty($this->groupSelect)) {
	$groupSelectBox = (new SelectHelper())->select('group')
		->options($this->groupSelect);
}
?>

<?php echo $this->render('files/View/Admin/Platby/Navigation.inc');?>
<?php header_main('Kategorie plateb');?>
<?php header_minor($this->action == 'add' ? 'Přidat specifický symbol' : 'Upravit specifický symbol');?>
<form action="" method="post">
	<?php echo $mainForm;?>
	<button name="action" value="confirm">Uložit</button>&nbsp;<a href="/admin/platby/structure">Zpět</a><br/><br/>
</form>
<?php if($this->action != 'add'): ?>
	<?php header_minor('Kategorie');?>
	<?php if(isset($groupForm)) echo $groupForm;?>
	<form action="" method="post">
		<span class="big">Přidat:</span>
		<a href="/admin/platby/structure/group/add?category=<?php echo $this->id;?>">novou kategorii</a>
		<?php if(isset($groupSelectBox)): ?>
			nebo existující: <?php echo $groupSelectBox;?>
			<button type="submit" name="action" value="group">odeslat</button>
		<?php endif;?>
	</form>
<?php endif;?>