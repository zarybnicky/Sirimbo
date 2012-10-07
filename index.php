<?php
include("files/Core/settings.php");
include("files/Core/log.php");
include("files/Core/form.php");
include("files/Core/cache.php");
include("files/Core/database.php");
include("files/Core/database/dbuser.php");
include("files/Core/user.php");
include("files/Core/view.php");
include("files/Core/permissions.php");
include("files/Core/mailer.php");
include("files/Core/debug.php");		//DEBUG ONLY!!!

//Are all CORE vars present?
if(!$sitemap_static || !$sitemap_dynamic || !class_exists("Database") || !class_exists("DBUser") ||
		!class_exists("User") || !class_exists("View") || !class_exists("Log") ||
		!class_exists("Permissions") || !class_exists("Mailer")) {
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
	
	foreach($parts as $key => $item)
		if(is_numeric($item))
			unset($parts[$key]);
	$parts = array_values($parts);
	
	$file = implode('/', $parts);
}

define('TISK', (get('view') == 'tisk') ? TRUE : FALSE);

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
