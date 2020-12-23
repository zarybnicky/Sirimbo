<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $uri], true)?>

<div class="container">
<span class="big">Přidat: </span>
<a href="/admin/platby/structure/group/add">kategorii</a>
<a href="/admin/platby/structure/category/add">specifický symbol</a>

<?= (new TableHelper($data))
    ->style('width:100%')
    ->column('buttons', '', '', 'width:70px')
    ->column('name', 'Název') ?>

<?php
if (!empty($orphanGroupSkupina)) {
    echo '<br/>';
    echo '<h3>Kategorie plateb bez přiřazené uživatelské skupiny</h3>';
    echo (new TableHelper($orphanGroupSkupina))
        ->style('width:50%')
        ->column('buttons', '', '', 'width:70px')
        ->column('name', 'Název');
}

if (!empty($orphanGroupCategory)) {
    echo '<br/>';
    echo '<h3>Kategorie plateb bez přiřazených specifických symbolů</h3>';
    echo (new TableHelper($orphanGroupCategory))
        ->style('width:50%')
        ->column('buttons', '', '', 'width:70px')
        ->column('name', 'Název');
}

if (!empty($orphanCategory)) {
    echo '<br/>';
    echo '<h3>Specifické symboly bez přiřazené kategorie</h3>';
    echo (new TableHelper($orphanCategory))
        ->style('width:50%')
        ->column('buttons', '', '', 'width:70px')
        ->column('name', 'Název');
}
?>
</div>
