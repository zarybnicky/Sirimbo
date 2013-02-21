<?php
class Controller_Admin_Permissions implements Controller_Interface {
	function _construct() {
		Permissions::checkError('permissions', P_ADMIN);
	}
	function view($id = null) {
		switch(post('action')) {
			case 'edit':
				$data = post('permissions');
				if($data[0])
					View::redirect('/admin/permissions/edit/' . $data[0]);
				break;
			case 'remove':
				if(!is_array(post('permissions')))
					break;
				$url = '/admin/permissions/remove?';
				foreach(post('permissions') as $id)
					$url .= '&u[]=' . $id;
				View::redirect($url);
				break;
		}
		
		include('files/Admin/Permissions/Display.inc');
	}
	function add($id = null) {
		if(empty($_POST) || is_object($f = $this->checkData($_POST, 'add'))) {
			include('files/Admin/Permissions/Form.inc');
			return;
		}
		$permissions = array();
		foreach(Settings::$permissions as $name => $item)
			$permissions[$name] = post($name);
		
		DBPermissions::addGroup(post('name'), post('description'), $permissions);
		
		View::redirect(post('referer') ? post('referer') : '/admin/permissions',
		'Úroveň úspěšně přidána');
	}
	function edit($id = null) {
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
		if(is_object($f = $this->checkData($_POST, 'edit'))) {
			include('files/Admin/Permissions/Form.inc');
			return;
		}
		$permissions = array();
		foreach(Settings::$permissions as $name => $item)
			$permissions[$name] = post($name);
		
		DBPermissions::editGroup($id, post('name'), post('description'), $permissions);
		
		View::redirect(post('referer') ? post('referer') : '/admin/permissions',
		'Oprávnění úspěšně upravena');
	}
	function remove($id = null) {
		if(!is_array(post('permissions')) || post('action') != 'confirm') {
			include('files/Admin/Permissions/DisplayRemove.inc');
			return;
		}
		foreach(post('permissions') as $id) {
			DBPermissions::removeGroup($id);
		}
		View::redirect('/admin/permissions',
			'Úrovně odebrány. Nezapomeňte přiřadit uživatelům z těchto skupin jinou skupinu!');
	}
	private function checkData($data, $action = 'add') {
		$f = new Form();
		foreach(Settings::$permissions as $name => $item) {
			$f->checkArrayKey(post($name), $item, 'Neplatná hodnota práva "' . $name . '"', $name);
			$permissions[$name] = post($name);
		}
		return $f->isValid() ? true : $f;
	}
}
?>