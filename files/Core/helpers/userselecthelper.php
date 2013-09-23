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
	
	public function userSelect($name = null) {
		$this->_defaultValues();
		if($name !== null)
			return $this->name($name);
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
	public function tmpSwitch($value) {
		$this->tmpSwitch = (bool) $value;
		return $this;
	}
	public function render() {
		$name = 'userselect' . rand(0,1024);
		
		$out = '<div class="' . $name . '">' . "\n";
		
		$out .= '<select name="' . $this->name . '">' . "\n";
		if(!post($this->name))
			$out .= '<option value="none" selected="selected">--- žádný ---</option>' . "\n";
		else
			$out .= '<option value="none">--- žádný ---</option>' . "\n";
		
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
			$out .= $user[$this->prijmeni] . ', ' . $user[$this->jmeno] .
				(isset($user['u_narozeni']) ? (', ' . $year) : '');
			$out .= '</option>' . "\n";
		}
		$out .= '</select>' . "\n";
		
		if($this->tmpSwitch) {
			ob_start();
		?>
<noscript><br/><a href="/admin/users/temporary">Nový dočasný uživatel</a></noscript>
<div class="new" style="display:none;text-align:right;">
Jméno: <input type="text" name="jmeno" size="8" /><br/>
Příjmení: <input type="text" name="prijmeni" size="8" /><br/>
Datum narození:&nbsp;<br/>
<?php echo Helper::get()->date('narozeni'), '<br/>';?>
<button type="submit">Uložit</button>
</div>
<div class="loading" style="display:none;"><img src="/images/loading_bar.gif"/></div>
<script type="text/javascript">
(function($) {
	$(function() {
		if(typeof $.fn.tempUserSelect == "undefined" && typeof window.loadingUS == "undefined") {
			window.loadingUS = true;
			$.getScript("/scripts/tempUserSelect.js", function() {
				$(".<?php echo $name;?>").tempUserSelect("<?php echo $this->type;?>");
				delete window.loadingUS;
			});
		} else {
			$.delayed<?php echo $name ?> = function() {
				if(typeof window.loadingUS == "undefined" && typeof $.fn.tempUserSelect != "undefined") {
					$(".<?php echo $name;?>").tempUserSelect("<?php echo $this->type;?>");
				} else {
					setTimeout(function() {$.delayed<?php echo $name?>();}, 200);
				}
			};
			$.delayed<?php echo $name?>();
		}
	})
}) (jQuery);
</script>
		<?php
			$out .= ob_get_clean();
		}
		$out .= '</div>';
		
		return $out;
	}
	public function __toString() {
		return $this->render();
	}
}
?>