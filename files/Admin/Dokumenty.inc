<?php
User::checkPermissionsError(L_EDITOR);

header_main("Správa souborů");

if(empty($_POST)) {
	include("files/Admin/Dokumenty/Display.inc");
	return;
}

switch(getPostField("action")) {
	case "upload":
		if(!empty($_FILES)) {
			$fileUpload = $_FILES["file"]["tmp_name"];
			$fileName = $_FILES["file"]["name"];
			$fileName = str_replace(array("#", "$", "%", "&", "^", "*", "?"),
				array("No.", "Dolar", "Procento", "And", ""), $fileName);
			
			$path = "upload/" . time() . "." . pathinfo($fileName, PATHINFO_EXTENSION);
			
			if(!getPostField("name"))
				$_POST["name"] = $fileName;
			
			if(move_uploaded_file($fileUpload, $path)) {
				chmod ($path, 0666);
				DBDokumenty::addDokument($path, getPostField("name"), $fileName,
					getPostField("kategorie"), User::getUserID());
				notice("Zmákli jsme to :o)");
			} else {
				notice("Bohužel, zkus to znova :o(");
			}
		}
		break;
		
	case "edit":
		$doku = getPostField("rozpis");
		if($doku[0]) {
			$file = DBDokumenty::getSingleDocument($doku[0]);
			
			if(!User::checkPermissionsBool(L_ADMIN) && 
					User::getUserID() != $file["d_kdo"])
				View::viewError(ER_AUTHORIZATION);
?>
<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
Staré jméno:&nbsp;<?php echo $file["d_name"];?><br />
Nové jméno:&nbsp;
<input type="text" name="newname" value="<?php echo $file["d_name"]; ?>" />
<input type="hidden" name="id" value="<?php echo $doku[0]; ?>" />
<button type="submit" name="action" value="edit_confirm">Upravit</button>
</form>
<?php
		}
		break;
	
	case "edit_confirm":
		$id = getPostField("id");
		$newname = getPostField("newname");
		
		if($newname == "")
			$newname = "---EMPTY---";
		
		if(!User::checkPermissionsBool(L_ADMIN) && 
				User::getUserID() != DBDokumenty::getDokumentUserID($id))
			View::viewError(ER_AUTHORIZATION);
		
		DBDokumenty::editDokument($id, $newname);
		notice("Příspěvek úspěšně upraven");
		
		break;

	case "remove":
		if(is_array(getPostField("dokumenty"))) {
			foreach(getPostField("dokumenty") as $item) {
				if(User::checkPermissionsBool(L_ADMIN) ||
						(User::getUserID() == DBDokumenty::getDokumentUserID($item))) {
					unlink(DBDokumenty::getDocumentPath($item));
					DBDokumenty::removeDokument($item);
				} else {
					View::viewError(ER_AUTHORIZATION);
				}
			}
			notice("Dokumenty odebrány");
		}
		//TODO: dokumenry - remove_confirm
		break;
}

include("files/Admin/Dokumenty/Display.inc");
return;
?>
