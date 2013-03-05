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
	
	public function __construct() {
		return $this->userSelect();
	}
	
	public function userSelect() {
		$this->_defaultValues();
		return $this;
	}
	private function _defaultValues() {
		$this->name = "user";
		$this->idVar = "u_id";
		$this->jmeno = "u_jmeno";
		$this->prijmeni = "u_prijmeni";
		$this->users = array();
		$this->type = 'user';
		$this->tmpVar = 'u_temporary';
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
	public static function tmpSwitch($value) {
		$this->tmpSwitch = (bool) $value;
		return $this;
	}
	public function render() {
		$name = 'userselect' . rand(0,1024);
		
		$out = '<div class="' . $name . '">' . "\n";
		if($this->tmpSwitch) {
			$out .=
"<script type=\"text/javascript\">
$(function(){
var name = '.' + '$name';
var type = '{$this->type}';

$(name + ' select').change(function() {
	if($(name + ' select').val() === 'temporary') {
		$(name + ' .new').slideDown();
	} else {
		$(name + ' .new').slideUp();
	}
});
$(name + ' .new button').click(function(){
	var celejmeno = $(name + ' .jmeno').val() + ' ' + $(name + ' .prijmeni').val();
	if(celejmeno == ' ') {
		return false;
	}
	$(name + ' option').each(function(){
		if(this.text == celejmeno || this.text == (celejmeno + ', ' + $(name + ' .year').val())) {
			$(name + ' .new').slideUp();
			$(name + ' select').val(this.value);
			return false;
		}
	});
	$.ajax({
		type: 'POST',
		url: '/admin/users/temporary?ajax=ajax',
		data: {
			jmeno: $(name + ' .jmeno').val(),
			prijmeni: $(name + ' .prijmeni').val(),
			narozeni: ($(name + ' .year').val() + '-' + $(name + ' .month').val() + '-' + $(name + ' .day').val())
		},
		beforeSend: function(){ $(name + ' .new').slideUp();$(name + ' .loading').slideDown();},
		success: function(data){
			data = JSON.parse(data);
			var id = (type == 'par') ? data.par_id : data.user_id;
			var fullname = data.jmeno + ' ' + data.prijmeni;
			
			$(name + ' select').append('<option value=\"' + id + '\" selected=\"selected\">' +
				fullname + ', ' + data.rok + '</option>');
			$(name + ' select').val(id);
			$(name + ' .loading').slideUp();
		}
	});
	return false;
});
});
</script>";
		}
		
		$out .= '<select name="' . $this->name . '">' . "\n";
		if(!post($this->name))
			$out .= '<option value="none" selected="selected">--- žádny ---</option>' . "\n";
		else
			$out .= '<option value="none">--- žádny ---</option>' . "\n";
		
		if($this->tmpSwitch)
			$out .= '<option value="temporary">--- dočasný ---</option>' . "\n";
		
		foreach($this->users as $user) {
			if(isset($user['u_narozeni']))
				list($year, $month, $day) = explode('-', $user['u_narozeni']);
			
			$id = $user[$this->idVar];
			if(post($this->name) == $id)
				$out .= '<option value="' . $id . '" selected="selected">';
			else
				$out .= '<option value="' . $id . '">';
			$out .= $user[$this->jmeno] . ' ' . $user[$this->prijmeni] .
				(isset($user['u_narozeni']) ? (', ' . $year) : '');
			$out .= '</option>' . "\n";
		}
		$out .= '</select>' . "\n";
		
		if($this->tmpSwitch) {
			$out .= '<noscript><br/><a href="/admin/users/temporary">Nový dočasný uživatel</a></noscript>';
			$out .= '<div class="new" style="display:none;text-align:right;">';
			$out .= 'Jméno: <input type="text" class="jmeno" size="8" /><br/>';
			$out .= 'Příjmení: <input type="text" class="prijmeni" size="8" /><br/>';
			$out .= 'Datum narození:&nbsp;<br/>';
			$out .= echoDateSelect('" class="day', '" class="month', '" class="year', 1940, 1) . '<br/>';
			$out .= '<button type="submit" name="jmeno">Uložit</button>';
			$out .= '</div>';
			$out .= '<div class="loading" style="display:none;"><img src="/images/loading_bar.gif"/></div>';
		}
		$out .= '</div>';
		
		return $out;
	}
	public function __toString() {
		return $this->render();
	}
}
?>