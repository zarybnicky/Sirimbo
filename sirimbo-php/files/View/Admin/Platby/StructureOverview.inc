<?= new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri])?>

<div class="container">
<span class="big">Přidat: </span>
<a href="/admin/platby/structure/group/add">kategorii</a>
<a href="/admin/platby/structure/category/add">specifický symbol</a>

<?= (new TableHelper($this->data))
    ->style('width:100%')
    ->column('buttons', '', '', 'width:50px')
    ->column('name', 'Název') ?>

<?php
if (!empty($this->orphanGroupSkupina)) {
    echo '<br/>';
    echo '<h3>Kategorie plateb bez přiřazené uživatelské skupiny</h3>';
    echo (new TableHelper($this->orphanGroupSkupina))
        ->style('width:50%')
        ->column('buttons', '', '', 'width:50px')
        ->column('name', 'Název');
}

if (!empty($this->orphanGroupCategory)) {
    echo '<br/>';
    echo '<h3>Kategorie plateb bez přiřazených specifických symbolů</h3>';
    echo (new TableHelper($this->orphanGroupCategory))
        ->style('width:50%')
        ->column('buttons', '', '', 'width:50px')
        ->column('name', 'Název');
}

if (!empty($this->orphanCategory)) {
    echo '<br/>';
    echo '<h3>Specifické symboly bez přiřazené kategorie</h3>';
    echo (new TableHelper($this->orphanCategory))
        ->style('width:50%')
        ->column('buttons', '', '', 'width:50px')
        ->column('name', 'Název');
}
?>
</div>
