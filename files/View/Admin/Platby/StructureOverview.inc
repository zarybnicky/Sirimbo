<?= $this->render('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri])?>

<div class="container">
<span class="big">Přidat: </span>
<a href="/admin/platby/structure/group/add">kategorii</a>
<a href="/admin/platby/structure/category/add">specifický symbol</a>

<?= $this->table()
    ->style('width:100%')
    ->column('buttons', '', '', 'width:50px')
    ->column('name', 'Název')
    ->data($this->data) ?>

<?php
if (!empty($this->orphanGroupSkupina)) {
    echo '<br/>';
    echo '<h3>Kategorie plateb bez přiřazené uživatelské skupiny</h3>';
    echo $this->table()
        ->style('width:50%')
        ->column('buttons', '', '', 'width:50px')
        ->column('name', 'Název')
        ->data($this->orphanGroupSkupina);
}

if (!empty($this->orphanGroupCategory)) {
    echo '<br/>';
    echo '<h3>Kategorie plateb bez přiřazených specifických symbolů</h3>';
    echo $this->table()
        ->style('width:50%')
        ->column('buttons', '', '', 'width:50px')
        ->column('name', 'Název')
        ->data($this->orphanGroupCategory);
}

if (!empty($this->orphanCategory)) {
    echo '<br/>';
    echo '<h3>Specifické symboly bez přiřazené kategorie</h3>';
    echo $this->table()
        ->style('width:50%')
        ->column('buttons', '', '', 'width:50px')
        ->column('name', 'Název')
        ->data($this->orphanCategory);
}
?>
</div>
