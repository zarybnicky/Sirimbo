<?php
User::checkPermissionsError(L_ADMIN);

header_main('Správa oprávnění');
notice(View::getRedirectMessage());

switch(Request::getAction()) {
	case 'add':
		if(empty($_POST)) {
			include('files/Admin/Permissions/Form.inc');
			return;
		}
		$f = new Form();
		foreach(Settings::$permissions as $name => $item) {
			$f->checkArrayKey(post($name), $item, 'Neplatná hodnota práva "' . $name . '"', $name);
			$permissions[$name] = post($name);
		}
		if(!$f->isValid()) {
			include('files/Admin/Permissions/Form.inc');
			return;
		}
		DBPermissions::addGroup(post('name'), post('description'), $permissions);
		
		View::redirect(post('referer') ? post('referer') : '/admin/permissions',
			'Úroveň úspěšně přidána');
		break;
	
	case 'edit':
		$id = Request::getID();
		if(!$id || !($data = DBPermissions::getSingleGroup($id)))
			View::redirect(post('referer') ? post('referer') : '/admin/permissions',
				'Skupina s takovým ID neexistuje');
		
		if(empty($_POST)) {
			post('name', $data['pe_name']);
			post('description', $data['pe_description']);
			foreach(Settings::$permissions as $name => $item) {
				post($name, $data['pe_' . $name]);
			}
			
			include('files/Admin/Permissions/Form.inc');
			return;
		}
		$f = new Form();
		foreach(Settings::$permissions as $name => $item) {
			$f->checkArrayKey(post($name), $item, 'Neplatná hodnota práva "' . $name . '"', $name);
			$permissions[$name] = post($name);
		}
		if(!$f->isValid()) {
			include('files/Admin/Permissions/Form.inc');
			return;
		}

		DBPermissions::editGroup($id, post('name'), post('description'), $permissions);
		
		View::redirect(post('referer') ? post('referer') : '/admin/permissions',
			'Oprávnění úspěšně upravena');
		break;
}

if(empty($_POST)) {
	include('files/Admin/Permissions/Display.inc');
	return;
}

switch(post('action')) {
	case 'edit':
		$data = post('permissions');
		if($data[0]) {
			header('Location: /admin/permissions/edit/' . $data[0]);
			return;
		}
		break;
	case 'remove':
		if(!is_array(post('permissions'))) {
			include('files/Admin/Permissions/Display.inc');
			return;
		}
		echo '<form action="/admin/permissions" method="POST">';
		notice('Bude nutné přiřadit uživatelům z těchto skupin jinou skupinu!');
		echo 'Opravdu chcete odstranit uživatelské úrovně:<br/><br/>';
		foreach(post('permissions') as $id) {
			$data = DBPermissions::getSingleGroup($id);
			echo $data['pe_name'], '<br />';
			echo '<input type="hidden" name="permissions[]" value="', $id, '" />';
		}
		echo '<br/>';
		echo '<button type="submit" name="action" value="remove_confirm">Odstranit</button>';
		echo '</form>';
		echo '<a href="/admin/permissions">Zpět</a>';
		return;

	case 'remove_confirm':
		if(!is_array(post('permissions'))) {
			include('files/Admin/Permissions/Display.inc');
			return;
		}
		foreach(post('permissions') as $id) {
			DBPermissions::removeGroup($id);
		}
		
		notice('Úrovně odebrány odebrány. Nezapomeňte přiřadit uživatelům z těchto skupin jinou skupinu!');
		include('files/Admin/Permissions/Display.inc');
		return;
}

include('files/Admin/Permissions/Display.inc');
?>