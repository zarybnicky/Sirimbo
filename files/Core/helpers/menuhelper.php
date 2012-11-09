<?php
class MenuHelper {
	private $left;
	private $right;
	private $float;
	private $content;
	
	const FLOAT_NONE = '0';
	const FLOAT_LEFT = 1;
	const FLOAT_RIGHT = 2;
	
	function menu() {
		$this->_defaultValues();
		return $this;
	}
	function _defaultValues() {
		$this->left = -1;
		$this->right = 0;
		$this->float = 0;
		$this->content = array();
	}
	function left($left) {
		if($left >= -1)
			$this->left = $left;
		return $this;
	}
	function right($right) {
		if($right >= -1)
			$this->right = $right;
		return $this;
	}
	function float($type) {
		if(is_int($type))
			$this->float = $type;
		return $this;
	}
	function content($name, $url, $button = FALSE, $replace = FALSE) {
		if(!$name || !$url)
			return $this;
		if($replace)
			$this->content = array($name => array($url, $button));
		else
			$this->content[$name] = array($url, $button);
		
		return $this;
	}
	function render() {
		$out = '<div style="position:block;width:150px;';
		if($this->right > -1) $out .= 'margin-left:' . $this->right . 'px;';
		if($this->left > -1) $out .= 'margin-right:' . $this->left . 'px;';
		switch($this->float) {
			case MenuHelper::FLOAT_RIGHT: $out .= 'float:right;'; break;
			case MenuHelper::FLOAT_LEFT: $out .= 'float:left;'; break;
			case MenuHelper::FLOAT_NONE: $out .= 'float:none;'; break;
		}
		$out .= '">';
		$out .= '<div style="position:fixed;z-index:100;width:inherit;' . 
			'border:1px solid #FFD390;background:#FFE7C7;margin-top:2px;padding:3px 1px;">';
		if(!empty($this->content)) {
			$i = 1;
			foreach($this->content as $name => $data) {
				if($data[1])
					$out .= '<button style="padding:0" name="action" value="' . $data[0] .
						'">' . $name . '</button>';
				else
					$out .= '<a style="padding:0 3px" href="' . $data[0] . '">' . $name . '</a>';
				if($i++ < count($this->content))
					$out .= '<br/>';
			}
		}
		$out .= '</div>';
		$out .= '</div>';
		return $out;
	}
	function __toString() {
		return $this->render();
	}
}