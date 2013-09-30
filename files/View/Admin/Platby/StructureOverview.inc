<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
echo '<h2>Struktura plateb</h2>';

echo '<span class="big">Přidat: </span> ';
echo '<a href="/admin/platby/structure/group/add">kategorii</a>&nbsp;';
echo '<a href="/admin/platby/structure/category/add">specifický symbol</a>';

echo $this->table()
	->style('width:100%')
		->column('buttons', '', '', 'width:50px')
	->column('name', 'Název')
	->column('colorBox', 'Skupiny')
	->column('validDate', 'Platí pro období')
	->data($this->data);

if(!empty($this->orphanGroupSkupina)) {
	echo '<br/>';
	echo '<h3>Kategorie plateb bez přiřazené uživatelské skupiny</h3>';
	echo $this->table()
		->style('width:50%')
		->column('buttons', '', '', 'width:50px')
		->column('name', 'Název')
		->data($this->orphanGroupSkupina);
}

if(!empty($this->orphanGroupCategory)) {
	echo '<br/>';
	echo '<h3>Kategorie plateb bez přiřazených specifických symbolů</h3>';
	echo $this->table()
	->style('width:50%')
	->column('buttons', '', '', 'width:50px')
	->column('name', 'Název')
	->data($this->orphanGroupCategory);
}

if(!empty($this->orphanCategory)) {
	echo '<br/>';
	echo '<h3>Specifické symboly bez přiřazené kategorie</h3>';
	echo $this->table()
		->style('width:50%')
		->column('buttons', '', '', 'width:50px')
		->column('name', 'Název')
		->data($this->orphanCategory);
}
?>