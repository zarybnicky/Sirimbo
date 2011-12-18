<?php
include("files/Core/settings.php");
include("files/Core/log.php");
include("files/Core/form.php");
include("files/Core/database.php");
include("files/Core/database/dbuser.php");
include("files/Core/user.php");
include("files/Core/view.php");
include("files/Core/permissions.php");
include("files/Core/debug.php");		//DEBUG ONLY!!!

//Are all CORE vars present?
if(!$sitemap_static || !$sitemap_dynamic || !class_exists("Database") || !class_exists("DBUser") ||
		!class_exists("User") || !class_exists("View") || !class_exists("Log") ||
		!class_exists("Permissions")) {
	if(class_exists("View")) {
		View::viewDynamic("files/Error/KeyFileCorrupt.inc");
		die();
	} else {
		die("Poškozené knihovní soubory");
	}
}

if(!isset($_GET["file"]) || $_GET["file"] == null) {
	$file = "home";
} else {
	$parts = explode('/', $_GET['file']);
	if(is_numeric($parts[count($parts) - 1]))
		array_pop($parts);
	$file = implode('/', $parts);
}

if(array_key_exists($file, $sitemap_static)) {
	$file = $sitemap_static[$file];
	if(file_exists($file)) {
	// OR LOCKED (DB)
		View::viewStatic($file);
	} else {
		View::viewStatic("files/Error/NotFoundRight.inc");
	}
} else if(array_key_exists($file, $sitemap_dynamic)) {
	$file = $sitemap_dynamic[$file];
	if(file_exists($file)) {
	// OR LOCKED (DB)
		View::viewDynamic($file);
	} else {
		View::viewStatic("files/Error/NotFoundRight.inc");
	}
} else {
	View::viewStatic("files/Error/NotPossible.inc");
}
?>
