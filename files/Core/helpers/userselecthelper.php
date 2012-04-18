<?php
class UserSelectHelper {
	private $name;
	private $idVar;
	private $jmeno;
	private $prijmeni;
	private $permissions;
	private $users;
	private $type;
	private $tmpVar;
	private $tmpString;
	private $tmpSwitch;
	
	public function userSelect() {
		$this->_defaultValues();
		return $this;
	}
	private function _defaultValues() {
		$this->name = "user";
		$this->idVar = "u_id";
		$this->jmeno = "u_jmeno";
		$this->prijmeni = "u_prijmeni";
		$this->permissions = L_USER;
		$this->users = array();
		$this->type = 'user';
		$this->tmpVar = 'u_temporary';
		$this->tmpString = 'D - ';
		$this->tmpSwitch = true;
	}
	public function name($name) {
		$this->name = $name;
		return $this;
	}
	public function idVar($idVar) {
		$this->idVar = $idVar;
		return $this;
	}
	public function jmeno($jmeno) {
		$this->jmeno = $jmeno;
		return $this;
	}
	public function prijmeni($prijmeni) {
		$this->prijmeni = $prijmeni;
		return $this;
	}
	public function permissions($permissions) {
		$this->permissions = $permissions;
		return $this;
	}
	public function users(array $users) {
		if(is_array($users))
			$this->users = $users;
		return $this;
	}
	public function type($type) {
		if($type == 'user' || $type == 'par')
			$this->type = $type;
		return $this;
	}
	public function tmpVar($var) {
		$this->tmpVar = $var;
		return $this;
	}
	public function tmpString($string) {
		$this->tmpString = $string;
		return $this;
	}
	public function tmpSwitch($flag) {
		$this->tmpSwitch = (boolean) $flag;
		return $this;
	}
	public function render() {
		$name = 'userselect' . rand(0,1024);
		$tmpString = ($this->tmpSwitch === true) ? $this->tmpString : '';
		
		$out = '<div class="' . $name . '">' . "\n";
		$out .= '<script type="text/javascript">';
		$out .=
"$(function(){
var name = '.' + '$name';
var type = '{$this->type}';
var tmpString = '$tmpString';
var permissions = '{$this->permissions}';

$(name + ' select').change(function() {
	if($(name + ' select').val() === 'temporary') {
		$(name + ' .new').slideDown();
	} else {
		$(name + ' .new').slideUp();
	}
});
$(name + ' .new button').click(function(){
	if($(name + ' .jmeno').val() && $(name + ' .prijmeni').val()) {
		var celejmeno = $(name + ' .jmeno').val() + ' ' + $(name + ' .prijmeni').val();
		var exists = '';
		$(name + ' option').each(function(){
			if(this.text == celejmeno || this.text == tmpString + celejmeno) {
				exists = this.value;
				return false;
			}
		});
		if(exists !== '') {
			$(name + ' .new').slideUp();
			$(name + ' select').val(exists);
			return false;
		}
		$.ajax({
			type: 'POST',
			url: '/admin/users/temporary?ajax=ajax',
			data: {jmeno: $(name + ' .jmeno').val(),prijmeni: $(name + ' .prijmeni').val(),
				permissions: permissions},
			beforeSend: function(){ $(name + ' .new').slideUp();$(name + ' .loading').slideDown();},
			complete: function(){
				$(name + ' .loading').slideUp();
			},
			success: function(data){
				data = JSON.parse(data);
				var id = (type == 'par') ? data.par_id : data.user_id;
				var fullname = ((data.temporary == 1) ? tmpString : '') + data.jmeno + ' ' + data.prijmeni;
				
				$(name + ' select').append('<option value=\"' + id + '\" selected=\"selected\">' +
					fullname + '</option>');
				$(name + ' select').val(id);
			}
		});
	}
	return false;
});
});";
		$out .= '</script>';
		$out .= '<select name="' . $this->name . '">' . "\n";
		if(!getPostField($this->name))
			$out .= '<option value="none" selected="selected">--- žádny ---</option>' . "\n";
		else
			$out .= '<option value="none">--- žádny ---</option>' . "\n";
			
		$out .= '<option value="temporary">--- dočasný ---</option>' . "\n";
		
		foreach($this->users as $user) {
			$id = $user[$this->idVar];
			if(getPostField($this->name) == $id)
				$out .= '<option value="' . $id . '" selected="selected">';
			else
				$out .= '<option value="' . $id . '">';
			$out .= (($this->tmpSwitch === true && isset($user[$this->tmpVar]) && $user[$this->tmpVar]) ?
				$this->tmpString : '');
			$out .= $user[$this->jmeno] . ' ' . $user[$this->prijmeni];
			$out .= '</option>' . "\n";
		}
		$out .= '</select>' . "\n";
		
		$out .= '<noscript><br/><a href="/admin/users/temporary">Nový dočasný uživatel</a></noscript>';
		$out .= '<div class="new" style="display:none;">';
		$out .= '<input type="text" class="jmeno" size="8" />';
		$out .= '<input type="text" class="prijmeni" size="8" />';
		$out .= '<button type="submit" name="jmeno">Uložit</button>';
		$out .= '</div>';
		$out .= '<div class="loading" style="display:none;"><img src="/images/loading_bar.gif"/></div>';
		
		$out .= '</div>';
		
		return $out;
	}
	public function __toString() {
		return $this->render();
	}
}
?>