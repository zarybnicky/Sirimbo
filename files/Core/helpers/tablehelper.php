<?php
class TableHelper {
	private $name;
	private $data;
	private $columns;
	private $i;
	
	function table() {
		$this->_defaultValues();
		return $this;
	}
	function _defaultValues() {
		$this->name = '';
		$this->data = null;
		$this->columns = null;
		$this->i = 0;
	}
	
	function name($n) {
		$this->name = $n;
		return $this;
	}
	function data($d) {
		if(isset($d[0]) && is_array($d[0]))
			$this->data = $d;
		return $this;
	}
	function columns($columns, $overwrite = false) {
		if($overwrite)
			$this->columns = null;
		if(!is_array($columns))
			return $this;
		
		foreach($columns as $c) {
			if(!is_array($c))
				continue;
			
			if(isset($c[0]) && !function_exists($c[1]))
				$c[1] = create_function('$r, $i', $c[1]);
			
			if(isset($c[0]) && isset($c[1]) && function_exists($c[1]))
				$this->columns[] = array($c[0], $c[1], isset($c[2]) ? $c[2] : null);
		}
		return $this;
	}
	function column($name, $function_name, $style = null) {
		if(!function_exists($function_name))
			$function_name = create_function('$r, $i', $function_name);
		
		if(function_exists($function_name))
			$this->columns[] = array($name, $function_name, $style);
		return $this;
	}
	
	private function getCounter() { return ' ' . $this->i . '.'; }
	
	function __toString() {
		return $this->render();
	}
	function render() {
		if($this->data === null && $this->columns === null)
			return '';
		
		$out = '<table><thead>';
		foreach($this->columns as $column) {
			$out .= '<th>' . $column[0] . '</th>';
		}
		$out .= '</thead><tbody>';
		foreach($this->data as $row) {
			++$this->i;
			$out .= '<tr>';
			foreach($this->columns as $c) {
				$out .= '<td' . ($c[2] !== null ? (' style="' . $c[2] . '"') : '') . '>';
				$out .= $c[1]($row, $this->i);
				$out .= '</td>';
			}
			$out .= '</tr>';
		}
		$out .= '</tbody></table>';
		return $out;
	}
}