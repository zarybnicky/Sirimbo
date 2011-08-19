<?php
function corrupt() {
	if(class_exists("View")) {
		View::viewDynamic("files/Error/KeyFileCorrupt.inc");
		die();
	} else {
		die("Poškozené knihovní soubory");
	}
}
include("files/Core/Settings.php");
include("files/Core/Log.php");
include("files/Core/Form.php");
include("files/Core/Database.php");
include("files/Core/User.php");
include("files/Core/View.php");

if(!$sitemap_static || !$sitemap_dynamic || !class_exists("Database") ||
		!class_exists("User") || !class_exists("View") || !class_exists("Log"))
	corrupt();

if(!isset($_GET["file"]) || $_GET["file"] == null) {
	$file = "home";
} else {
	$file = $_GET["file"];
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