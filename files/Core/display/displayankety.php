<?php
class DisplayAnkety {
	public static function viewAnketa($data, $items, $active = true) {
		if($active)
			echo '<form action="/ankety" method="POST">';
		else
			echo $data['ak_jmeno'], ': ';
		echo '<b>', $data['ak_text'], '</b><br/>';
		foreach($items as $item) {
			if($active)
				echo '<input type="radio" name="choice" value="', $item['aki_id'], '" />';
			echo $item['aki_text'];
			if(!$active)
				echo ' - ', $item['aki_pocet'];
			echo '<br/>';
		}
		if($active) {
			echo '<button type="submit" name="id" value="', $data['ak_id'], '">Hlasovat</button>';
			echo '<a href="/ankety/', $data['ak_id'], '">Zobrazit</a>';
			echo '</form>';
		} else {
			echo '<br />';
		}
	}
	
	public static function viewAnkety($active = true, $visible = false, $ip = 0) {
		$whole = DBAnkety::getAnketyWithItems($visible, $ip);
		
		if(empty($whole)) {
			return false;
		}
		
		foreach($whole as $key => $row) {
			DisplayAnkety::viewAnketa($row, $row['items'], $active);
			if(count($whole)-1 != $key) {
				echo '<hr/>';
			}
		}
		return true;
	}
}
?>