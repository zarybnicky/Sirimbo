<?php
/*
 * Example:
$h = Helper::get();
echo '<form action="', Request::getURI(), '" method="post">';
echo $h->date('2012-12-21')->name('test1')->selectBox(), '<br/>';
echo $h->date('2000-01-01')->name('test2')->textBox(), '<br/>';
echo $h->date(false)->name('test1')->getPost(), '<br/>';
echo $h->date(false)->name('test2')->getPost(), '<br/>';
echo '<input type="submit" value="Send" />';
echo '</form>';
*/
class DateHelper {
	private $date;
	private $view;
	private $name;
	private $post;
	private $fromYear;
	private $toYear;
	
	function date($d = null) {
		$this->_defaultValues();
		
		if($d) $this->setDate($d);
		
		return $this;
	}
	function _defaultValues() {
		$this->view = 'select';
		$this->date = null;
		$this->name = null;
		$this->post = true;
		$this->fromYear = ((int) date('Y')) - 75;
		$this->toYear = ((int) date('Y')) + 5;
	}
	function setDate($d) {
		if(is_a($d, 'Date'))
			$this->date = $d;
		if(is_string($d)) {
			$this->date = new Date($d);
		}
		if(!is_a($this->date, 'Date') || !$this->date->isValid())
			$this->date = null;
		return $this;
	}
	function getDate() {
		return $this->date;
	}
	
	function selectBox() {
		$this->view = 'select';
		return $this;
	}
	function textBox() {
		$this->view = 'text';
		return $this;
	}
	function name($name) {
		if($name)
			$this->name = $name;
		return $this;
	}
	function post($post) {
		$this->post = (bool) $post;
		return $this;
	}
	function fromYear($y) {
		if($y > 1800)
			$this->fromYear = $y;
		return $this;
	}
	function toYear($y) {
		if($y < 2200 && $y > $this->fromYear)
			$this->toYear = $y;
		return $this;
	}
	
	function getPost() {
		if(post($this->name)) {
			return new Date(post($this->name));
		} elseif(post($this->name . '-year') && post($this->name . '-month') &&
				post($this->name . '-day')) {
			return new Date(post($this->name . '-year') . '-' .
				post($this->name . '-month') . '-' .
				post($this->name . '-day'));
		} elseif($this->date) {
			return $this->date;
		} else {
			return '';
		}
	}
	
	function __toString() {
		return $this->render();
	}
	function render() {
		$h = Helper::get();
		$out = '';
		
		if($this->view == 'select') {
			$h->select()
				->get(false);
			
			$h->select(false)
				->name($this->name . '-day')
				->value($this->date ? $this->date->getDay() : null)
				->options(array(), true)
				->option('00', 'Den');
			for($i = 1; $i < 32; $i++)
				$h->select(false)
					->option((($i < 10) ? ('0' . $i) : $i), $i);
			$out .= $h->select(false);
			
			$out .= $h->select(false)
				->name($this->name . '-month')
				->value($this->date ? $this->date->getMonth() : null)
				->options(array(), true)
				->option('00', 'Měsíc')
				->option('01', 'Leden')		->option('02', 'Únor')
				->option('03', 'Březen')	->option('04', 'Duben')
				->option('05', 'Květen')	->option('06', 'Červen')
				->option('07', 'Červenec')	->option('08', 'Srpen')
				->option('09', 'Září')		->option('10', 'Říjen')
				->option('11', 'Listopad')	->option('12', 'Prosinec');
			
			$h->select(false)
				->name($this->name . '-year')
				->value($this->date ? $this->date->getYear() : null)
				->options(array(), true)
				->option('0000', 'Rok');
			for($i = $this->fromYear; $i < $this->toYear; $i++)
				$h->select(false)
					->option($i, $i);
			$out .= $h->select(false);
		} elseif($this->view == 'text') {
			$selected = $this->post ? post($this->name) : get($this->name);
			if($selected) {
				$date = new Date($selected);
				$selected = $date->getDate(Date::FORMAT_SIMPLIFIED);
				unset($date);
			} else {
				$selected = $this->date ? $this->date->getDate(Date::FORMAT_SIMPLIFIED) : '';
			}
			
			$out .= '<input type="text" name="' . $this->name .
				'" value="' . $selected . '" />' . "\n";
		}
		return $out;
	}
}