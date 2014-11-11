<?php
function _isActive($href) {
  return strripos(Request::getLiteralURI(), $href) !== (strlen(Request::getLiteralURI()) - strlen($href));
}
function _getItem($name, $href) {
  $href = 'member/' . $href;
  if (_isActive($href)) {
    return '<a href="/' . $href . '">' . $name . '</a>';
  } else {
    return $name;
  }
}
if (TISK) {
  echo $this->header('Přehled členů');
  return;
}
?>
<style>
 #adminNavigation {
   display:inline;
   font-size: 50%;
 }
 #adminNavigation li {
   display:inline;
   font-size: 110%;
   padding: 0 5px;
   font-weight:bold;
 } #adminNavigation li a {
   font-weight:normal;
 }
</style>
<?=
$this->header(
  "Přehled členů" . 
  "<ul id=\"adminNavigation\" class=\"small\">" .
  "<li>(" . _getItem('Členové podle skupin', 'clenove/structure') . "</li>" .
  "<li>" . _getItem('Členové abecedně', 'clenove/seznam') . "</li>" .
  "<li>" . _getItem('Seznam skupin', 'clenove/skupiny') . ")</li>" .
  "</ul>"
) ?>

<?php echo $this->notice($this->redirect()->getMessage());?>
