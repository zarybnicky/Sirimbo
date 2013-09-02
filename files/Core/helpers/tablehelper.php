<?php
class TableHelper {
	private $name;
	private $data;
	private $columns;
	private $i;
	private $style;
	private $showHeader;
	
	function table() {
		$this->_defaultValues();
		return $this;
	}
	function _defaultValues() {
		$this->name = '';
		$this->data = array();
		$this->columns = array();
		$this->i = 0;
		$this->style = '';
		$this->showHeader = true;
	}
	
	function name($n) {
		$this->name = $n;
		return $this;
	}
	function style($s) {
		$this->style = ' style="' . $s . '"';
		return $this;
	}
	function data($d) {
		if($d instanceof Traversable || (isset($d[0]) && is_array($d[0])))
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
			call_user_func_array(array($this, 'column'), $c);
		}
		return $this;
	}
	function column($id, $name, $class = null, $style = null) {
		$html = ($class !== null ? (' class="' . $class . '"') : '') .
				($style !== null ? (' style="' . $style . '"') : '');
		
		$this->columns[] = array($id, $name, $html);
		return $this;
	}
	function showHeader($b) {
		$this->showHeader = (bool) $b;
		return $this;
	}
	
	private function getCounter() { return ' ' . $this->i . '.'; }
	
	function __toString() {
		return $this->render();
	}
	function render() {
		if($this->data === null && $this->columns === null)
			return '';
		ob_start();
		?>
<table<?php echo $this->style;?>>
	<?php if($this->showHeader): ?>
	<thead>
		<tr>
			<?php foreach($this->columns as $c): ?>
			<th><?php echo $c[1];?></th>
			<?php endforeach;?>
		</tr>
	</thead>
	<?php endif;?>
	<tbody>
		<?php foreach($this->data as $row): ++$this->i;if(!$row) continue;?>
		<tr>
			<?php foreach($this->columns as $c): ?>
			<td<?php echo $c[2];?>>
			<?php echo $row[$c[0]] == '{counter}' ? $this->getCounter() : $row[$c[0]];?>
			</td>
			<?php endforeach;?>
		</tr>
		<?php endforeach;?>
	</tbody>
</table>
		<?php
		return ob_get_clean();
	}
}
?>