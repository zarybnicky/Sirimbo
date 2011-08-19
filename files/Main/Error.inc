<h2>Chyba</h2>
<hr />
<?php
if($_GET["id"] == null)
	echo "Tato stránka není určena k přímému prohlížení. Prosím, přejděte <a href=\"/\">sem</a>.";
else if(!file_exists(Settings::$errors[$_GET["id"]]))
	echo "Chybová stránka s daným ID nebyla nalezena.";
else
	include(Settings::$errors[$_GET["id"]]);
?>
<hr />