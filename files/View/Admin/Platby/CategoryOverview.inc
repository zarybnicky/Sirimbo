<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
header_main('Kategorie plateb');

echo '<span class="big">Přidat: </span> ';
echo '<a href="/admin/platby/category/add_group">kategorii</a>&nbsp;';
echo '<a href="/admin/platby/category/add_category">specifický symbol</a>';

echo $this->table()
	->style('width:100%')
		->column('buttons', '', '', 'width:50px')
	->column('name', 'Název')
	->column('colorBox', 'Skupiny')
	->column('validDate', 'Platí pro období')
	->data($this->data);

if(!empty($this->orphanGroupSkupina)) {
	echo '<br/>';
	header_minor('Kategorie plateb bez přiřazené uživatelské skupiny');
	echo $this->table()
		->style('width:50%')
		->column('buttons', '', '', 'width:50px')
		->column('name', 'Název')
		->data($this->orphanGroupSkupina);
}

if(!empty($this->orphanGroupCategory)) {
	echo '<br/>';
	header_minor('Kategorie plateb bez přiřazených specifických symbolů');
	echo $this->table()
	->style('width:50%')
	->column('buttons', '', '', 'width:50px')
	->column('name', 'Název')
	->data($this->orphanGroupCategory);
}

if(!empty($this->orphanCategory)) {
	echo '<br/>';
	header_minor('Specifické symboly bez přiřazené kategorie');
	echo $this->table()
		->style('width:50%')
		->column('buttons', '', '', 'width:50px')
		->column('name', 'Název')
		->data($this->orphanCategory);
}
?>