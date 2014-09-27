<?php
function _isActive($href) {
    return strripos(Request::getLiteralURI(), $href) !== (strlen(Request::getLiteralURI()) - strlen($href));
}
function _getItem($name, $href) {
    $href = 'admin/' . $href;
    if (_isActive($href))
        echo '<a href="/' . $href . '">' . $name . '</a>';
    else
        echo $name;
}
echo '<h1>Správa plateb</h1>';
if (TISK)
    return;
?>
<style>
#adminNavigation li {
    display:inline;
    font-size: 115%;
    color: #572E00;
    padding: 0 5px;
    font-weight:bold;
} #adminNavigation li a {
    font-weight:normal;
}
</style>
<ul id="adminNavigation">
    <li><?php _getItem('Přehled', 'platby/overview');?></li>
    <li><?php _getItem('Hledání', 'platby/items');?></li>
    <?php if (!_isActive('platby/structure/category') || !_isActive('platby/structure/group') || !_isActive('platby/structure')) : ?>
        <li><?php _getItem('Struktura', 'platby/structure');?>:</li>
        <li>(<?php _getItem('Kategorie', 'platby/structure/group');?></li>
        <li><?php _getItem('Specifické symboly', 'platby/structure/category');?>)</li>
    <?php else:?>
        <li><?php _getItem('Struktura', 'platby/structure');?></li>
    <?php endif;?>
    <?php if (!_isActive('platby/raw') || !_isActive('platby/manual') || !_isActive('platby/discarded')) : ?>
        <li><?php _getItem('Import', 'platby/raw');?>:</li>
        <li>(<?php _getItem('Nezařazené platby', 'platby/manual');?></li>
        <li><?php _getItem('Vyřazené platby', 'platby/discarded');?>)</li>
    <?php else:?>
        <li><?php _getItem('Import', 'platby/raw');?></li>
    <?php endif;?>
</ul>
<?php echo $this->notice($this->redirect()->getMessage());?>
