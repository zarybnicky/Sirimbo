<?php
User::checkPermissionsError(L_ADMIN);
header_main("Správa uživatelů");

if(empty($_POST)) {
	include("files/Admin/Users/Display.inc");
	return;
}

switch(getPostField("action")) {
	
	case "edit":
		$users = getPostField("users");	
		if($users[0]) {
			if(!DBUser::isUserConfirmed($users[0])) {
				notice("Uživatel ještě není potvrzený - ", $users[0]);
				break;
			}
			$userData = DBUser::getUserData($users[0]);
			$_POST["login"] = $users[0];
			$_POST["level"] = $userData["u_level"];
			switch($userData["u_level"]) {
				case L_HOST:
					$_POST["level"] = "host";		break;
				case L_USER:
					$_POST["level"] = "user";		break;
				case L_EDITOR:
					$_POST["level"] = "editor";		break;
				case L_TRENER:
					$_POST["level"] = "trener";		break;
				case L_ADMIN:
					$_POST["level"] = "admin";		break;
				case L_SADMIN:
					$_POST["level"] = "sadmin";		break;
				default:
					$_POST["level"] = "user";		break;
			}
			$_POST["ban"] = $userData["u_ban"];
			$_POST["lock"] = $userData["u_lock"];
			$_POST["jmeno"] = $userData["u_jmeno"];
			$_POST["prijmeni"] = $userData["u_prijmeni"];
			$_POST["pohlavi"] = $userData["u_pohlavi"];
			$_POST["email"] = $userData["u_email"];
			$_POST["telefon"] = $userData["u_telefon"];
			$_POST["poznamky"] = $userData["u_poznamky"];
			include("files/Admin/Users/Form.inc");
			return;
		}
		break;
		
	case "add":
		include("files/Admin/Users/Form.inc");
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
					DBUser::getUserlevel($user) >= User::getUserLevel()) ||
					DBUser::isUserLocked($user))) {
				View::viewError(ER_AUTHORIZATION);
				break;
			}
		}
		$user_exists = DBUser::getUserID($user);
		
		$error = false;
		
		ob_start();
		include("files/Admin/Users/Form.inc");	
		$html = ob_get_clean();
		
		if(!$error) {
			if(getPostField("action") == "add_confirm" && $user_exists) {
				notice("Uživatel s takovým jménem už tu je");
				echo $html;
				return;
			}
			if(getPostField("action") == "edit_confirm" && !DBUser::isUserConfirmed($user)) {
				notice("Uživatel ještě není potvrzený");
				break;
			}
			
			switch(getPostField("level")) {
				case "host":
					$level = L_HOST;		break;
				case "user":
					$level = L_USER;		break;
				case "editor":
					$level = L_EDITOR;		break;
				case "trener":
					$level = L_TRENER;		break;
				case "admin":
					$level = L_ADMIN;		break;
				case "sadmin":
					if(getPostField("action") == "edit_confirm")
						$level = L_SADMIN;
					else
						$level = L_ADMIN;
					break;
				default:
					$level = L_HOST;	break;
			}
			if(getPostField("action") == "edit_confirm") {
				DBUser::setUserData($user, getPostField("jmeno"), getPostField("prijmeni"),
					getPostField("pohlavi"), getPostField("email"), getPostField("telefon"),
					getPostField("poznamky"), $level, getPostField("lock") ? 1 : 0,
					getPostField("ban") ? 1 : 0);
				notice("Uživatel úspěšně upraven");
			} else if(getPostField("action") == "add_confirm") {
				$pass = User::Crypt($_POST["pass"]);
				
				DBUser::addUser($user, $pass, getPostField("jmeno"), getPostField("prijmeni"),
					getPostField("pohlavi"), getPostField("email"), getPostField("telefon"),
					getPostField("poznamky"), $level, getPostField("lock") ? 1 : 0,
					getPostField("ban") ? 1 : 0, "1");
				notice("Uživatel úspěšně přidán");
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
						DBUser::getUserlevel($user) < User::getUserLevel() && !DBUser::isUserLocked($user)))
					DBUser::removeUser($user);
				else
					View::viewError(ER_AUTHORIZATION);
			}			
			notice("Uživatelé odebráni");
		}
		break;
	
	case "confirm":	
		include("files/Admin/Users/DisplayNew.inc");
		return;
		break;
	
	case "confirm_confirm":
		if(is_array(getPostField("users"))) {
			foreach(getPostField("users") as $login) {
				$old_level = getPostField($login . '-level');
				$level = ($old_level == 'host' || $old_level == 'user' ||
						$old_level == 'editor' || $old_level == 'trener' || $old_level == 'admin') ?
					$old_level : 'host';
				DBUser::confirmUser($login, $level);
			}
		}
		notice("Uživatelé potvrzeni");
		include("files/Admin/Users/DisplayNew.inc");
		return;
		break;
}

include("files/Admin/Users/Display.inc");
?>