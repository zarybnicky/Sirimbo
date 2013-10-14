<?php
class ColorSelectHelper {
	private $field;
	private $value;
	
	function colorselect($field = null, $value = null) {
		$this->_defaultValues();
		if($field !== null)
			$this->field = $field;
		if($value !== null)
			$this->value = $value;
		return $this;
	}
	private function _defaultValues() {
		$this->field = '';
		$this->value = '';
	}
	function field($field) {
		$this->field = $field;
		return $this;
	}
	function value($value) {
		$this->value = $value;
		return $this;
	}
	function __toString() {
		return $this->render();
	}
	function render() {
		if(!$this->value && post($this->field))
			$this->value = post($this->field);
		
		return
		"<script src='/scripts/spectrum.js'></script>
    	<link rel='stylesheet' href='/style/spectrum.css' />
		<input id='color-{$this->field}' type='color' name='{$this->field}' value='{$this->value}' />
		<script>$('#color-{$this->field}').spectrum({color:'{$this->value}',showInput:true,
			clickoutFiresChange:true,preferredFormat:'hex6'});</script>";
	}
}