<?php
function _isActive($href) {
    return strripos(Request::getLiteralURI(), $href) !== (strlen(Request::getLiteralURI()) - strlen($href));
}
function _getItem($name, $href) {
    $href = 'member/' . $href;
    if (_isActive($href)) {
        echo '<a href="/' . $href . '">' . $name . '</a>';
    } else {
        echo $name;
    }
}
if (TISK) {
    echo '<h1>Přehled členů</h1>';
}
?>
<style>
#adminNavigation {
    display:inline;
}
#adminNavigation li {
    display:inline;
    font-size: 110%;
    color: #572E00;
    padding: 0 5px;
    font-weight:bold;
} #adminNavigation li a {
    font-weight:normal;
}
</style>
<div style="margin-bottom:0.5em;">
    <h1 style="display:inline;">Přehled členů</h1>
    <ul id="adminNavigation">
        <li>(<?php _getItem('Členové podle skupin', 'clenove/structure');?></li>
        <li><?php _getItem('Členové abecedně', 'clenove/seznam');?></li>
        <li><?php _getItem('Seznam skupin', 'clenove/skupiny');?>)</li>
    </ul>
</div>
<?php echo $this->notice($this->redirect()->getMessage());?>
