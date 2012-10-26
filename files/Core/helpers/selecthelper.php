<?php
class SelectHelper {
	private $name;
	private $value;
	private $options;
	private $get;
	
	function select() {
		$this->_defaultValues();
		return $this;
	}
	function _defaultValues() {
		$this->name = '';
		$this->value = NULL;
		$this->options = array();
		$this->get = true;
	}
	
	function name($name = '') {
		if($name)
			$this->name = $name;
		return $this;
	}
	function value($value = NULL) {
		if($value)
			$this->value = $value;
		return $this;
	}
	function option($value, $name) {
		if(isset($value) && isset($name))
			$this->options[$value] = $name;
		return $this;
	}
	function options($options = array(), $overwrite = false) {
		if($overwrite === true)
			$this->options = array();
		
		if(!is_array($options) || empty($options))
			return $this;
		
		foreach($options as $value => $name) {
			if(!isset($name))
				continue;
			$this->options[$value] = $name;
		}
		return $this;
	}
	function get($get = true) {
		$this->get = (bool) $get;
		return $this;
	}
	
	function __toString() {
		return $this->render();
	}
	function render() {
		$out = '<select name="' . $this->name . '">' . "\n";
		if(!empty($this->options)) {
			if($this->value)
				$selected = $this->value;
			else
				$selected = $this->get ? get($this->name) : post($this->name);
				
			foreach($this->options as $value => $name) {
				$out .= '<option value="' . $value . '"' .
					($selected == $value ? ' selected="selected"' : '') .
					'>' . $name . '</option>' . "\n";
			}
		}
		$out .= '</select>' . "\n";
		
		return $out;
	}
}
?>