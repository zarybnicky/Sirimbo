<?= $this->header('Správa plateb') ?>
<?php
if (TISK) return;
$uri = $this->uri;
$isActive = function ($href) use ($uri) {
    return strripos($uri, $href) !== (strlen($uri) - strlen($href));
};
$getItem = function ($name, $href) use ($isActive) {
    $href = 'admin/' . $href;
    if ($isActive($href)) {
        return '<a href="/' . $href . '">' . $name . '</a>';
    } else {
        return $name;
    }
}
?>
<style>
#adminNavigation li {
    display:inline;
    font-size: 115%;
    padding: 0 5px;
    font-weight:bold;
} #adminNavigation li a {
    font-weight:normal;
}
</style>
<div class="container">
<ul id="adminNavigation">
    <li><?= $getItem('Přehled', 'platby/overview') ?></li>
    <li><?= $getItem('Hledání', 'platby/items') ?></li>
    <?php if (!$isActive('platby/structure/category') || !$isActive('platby/structure/group') || !$isActive('platby/structure')) : ?>
        <li><?= $getItem('Struktura', 'platby/structure') ?>:</li>
        <li>(<?= $getItem('Kategorie', 'platby/structure/group') ?></li>
        <li><?= $getItem('Specifické symboly', 'platby/structure/category') ?>)</li>
    <?php else: ?>
        <li><?= $getItem('Struktura', 'platby/structure') ?></li>
    <?php endif ?>
    <?php if (!$isActive('platby/raw') || !$isActive('platby/manual') || !$isActive('platby/discarded')) : ?>
        <li><?= $getItem('Import', 'platby/raw') ?>:</li>
        <li>(<?= $getItem('Nezařazené platby', 'platby/manual') ?></li>
        <li><?= $getItem('Vyřazené platby', 'platby/discarded') ?>)</li>
    <?php else: ?>
        <li><?= $getItem('Import', 'platby/raw') ?></li>
    <?php endif ?>
</ul>
</div>
