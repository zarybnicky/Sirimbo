<?php
User::checkPermissionsError(L_ADMIN);
echo "<div class=\"h_section\">Správa uživatelů</div>";

var_dump($_POST);

if(empty($_POST)) {
	include("files/Admin/UsersDisplay.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit":
		if(is_array(getPostField("users"))) {
			foreach(getPostField("users") as $user) {
				$userData = Database::getUserData($user);
				$_POST["login"] = $user;
				$_POST["level"] = $userData["u_level"];
				switch($userData["u_level"]) {
					case L_USER:
						$_POST["level"] = "user";	break;
					case L_EDITOR:
						$_POST["level"] = "editor";	break;
					case L_TRENER:
						$_POST["level"] = "trener";	break;
					case L_ADMIN:
						$_POST["level"] = "admin";	break;
					default:
						$_POST["level"] = "user";	break;
				}
				$_POST["ban"] = $userData["u_ban"];
				$_POST["lock"] = $userData["u_lock"];
				$_POST["jmeno"] = $userData["u_jmeno"];
				$_POST["prijmeni"] = $userData["u_prijmeni"];
				$_POST["email"] = $userData["u_email"];
				$_POST["telefon"] = $userData["u_telefon"];
				$_POST["poznamky"] = $userData["u_poznamky"];
				include("files/Admin/UsersForm.inc");
			}
			return;
		}
		break;
		
	case "add":
		include("files/Admin/UsersForm.inc");
		return;

	case "remove":
		if(is_array(getPostField("users"))) {
			echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"POST\">";
			echo "Opravdu chcete odstranit uživatele:<br/><br/>";
			foreach(getPostField("users") as $user) {
				echo "\t$user<br />";
				echo "<input type=\"hidden\" name=\"users[]\" value=\"$user\" />";
			}
			echo "<br/>";
			echo "<button type=\"submit\" name=\"action\" value=\"remove_confirm\"" .
				">Odstranit</button>";
			echo "</form>";
			echo "<form action=\"{$_SERVER["REQUEST_URI"]}\" method=\"GET\">";
			echo "<button type=\"submit\">Zpět</button>";
			echo "</form>";
			return;
		}
		break;

	case "edit_confirm":
	case "add_confirm":
		$user = getPostField("login");
		if(getPostField("action") == "edit_confirm") {
			if(!User::checkPermissionsBool(L_SADMIN) && ((
					User::getUserName() != $user &&
					Database::getUserlevel($user) >= User::getUserLevel()) ||
					Database::isUserLocked($user))) {
				View::viewError(ER_AUTHORIZATION);
				break;
			}
		}
		$error = false;
		
		ob_start();
		include("files/Admin/UsersForm.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			switch(getPostField("level")) {
				case "user":
					$level = L_USER;	break;
				case "editor":
					$level = L_EDITOR;	break;
				case "trener":
					$level = L_TRENER;	break;
				case "admin":
					$level = L_ADMIN;	break;
				default:
					$level = L_USER;	break;
			}
			if(getPostField("action") == "edit_confirm") {
				User::editUser($user, $level, (getPostField("lock")) ? 1 : 0,
					(getPostField("ban")) ? 1 : 0, getPostField("jmeno"), getPostField("prijmeni"),
					getPostField("email"), getPostField("telefon"), getPostField("poznamky"));
				echo "Uživatel úspěšně upraven.<br/>";
			} else if(getPostField("action") == "add_confirm") {
				$pass = md5($_POST["pass"]);
				//TODO: Add custom algorithm
				//TODO: Changing passwords
				User::addUser($user, $pass, $level, (getPostField("lock")) ? 1 : 0,
					(getPostField("ban")) ? 1 : 0, getPostField("jmeno"),
					getPostField("prijmeni"), getPostField("email"), getPostField("telefon"),
					getPostField("poznamky"));
				echo "Uživatel úspěšně přidán.<br/>";
			}
		} else {
			echo $html;
			return;
		}
		break;

	case "remove_confirm":
		if(is_array(getPostField("users"))) {
			foreach(getPostField("users") as $user) {
				if(User::checkPermissionsBool(L_SADMIN) || (User::getUserName() != $user &&
						Database::getUserlevel($user) < User::getUserLevel() &&	!Database::isUserLocked($user)))
					Database::removeUser($user);
				else
					View::viewError(ER_AUTHORIZATION);
			}			
			echo "Uživatel(é) odebrán(i).<br/>";
		}
		break;

	default:
		include("files/Admin/UsersDisplay.inc");
		break;
}

include("files/Admin/UsersDisplay.inc");
?>