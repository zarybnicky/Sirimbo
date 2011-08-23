<h2>Chyba</h2>
<hr />
<?php
if($_GET["id"] == null)
	notice("Tato stránka není určena k přímému prohlížení. Prosím, přejděte <a href=\"/\">sem</a>");
else if(!file_exists(Settings::$errors[$_GET["id"]]))
	notice("Chybová stránka s daným ID nebyla nalezena");
else
	include(Settings::$errors[$_GET["id"]]);
?>
<hr />