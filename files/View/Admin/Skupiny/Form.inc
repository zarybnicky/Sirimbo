<?php
header_main('Správa skupin');
header_minor($this->action == 'add' ? 'Přidat skupinu' : 'Upravit skupinu');
notice($this->redirect()->getMessage());

$colorSelect = array();
foreach(Settings::$barvy as $name => $barva)
	$colorSelect[$name] = $barva[0];

echo '<form action="" method="POST">';
echo $this->table()
	->style('width:100%')
	->showHeader(false)
	->column('desc', '', 'r')
	->column('value', '')
	->data(array(
			array(
					'desc' => 'Jméno',
					'value' => '<input type="text" name="name" value="' . post('name') . '" />'
			), array(
					'desc' => 'Barva',
					'value' => $this->select('color')->options($colorSelect)
			), array(
					'desc' => 'Popis',
					'value' => '<textarea rows="15" cols="50" name="desc">' . post('popis') . '</textarea>'
			)
	));
echo '<button type="submit" name="action" value="confirm">' .
	($this->action == 'add' ? 'Přidat' : 'Upravit') .
	'</button>&nbsp;<a href="/admin/skupiny">Zpět</a>';
echo '</form>';