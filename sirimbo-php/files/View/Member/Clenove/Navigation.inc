<?php
$uri = $this->uri;
$isActive = function ($href) use ($uri) {
    return strripos($uri, $href) !== (strlen($uri) - strlen($href));
};
$getItem = function ($name, $href) use ($isActive) {
    $href = 'member/' . $href;
    if ($isActive($href)) {
        return '<a href="/' . $href . '">' . $name . '</a>';
    } else {
        return $name;
    }
};
?>
<style>
 #adminNavigation li {
   display:inline;
   font-size: 110%;
   padding: 0 5px;
   font-weight:bold;
 } #adminNavigation li a {
   font-weight:normal;
 }
</style>
<div class="container">
    <ul id="adminNavigation" class="small">
        <li><?= $getItem('Členové podle skupin', 'clenove') ?></li>
        <li><?= $getItem('Členové abecedně', 'clenove/seznam') ?></li>
        <li><?= $getItem('Seznam skupin', 'clenove/skupiny') ?></li>
    </ul>
</div>
