<?php
class DisplayAnkety {
	public static function viewAnketa($data, $items, $active = true) {
		$sum = 0;
		foreach($items as $item) { $sum += $item['aki_pocet']; }
		if($sum == 0) $sum = 1;
		
		if($active)
			echo '<form action="/home/ankety" method="POST">';
		echo '<b>', $data['ak_text'], '</b>';
		if(Permissions::canEditAnketa($data['ak_kdo']))
			echo ' - <a href="/admin/ankety/edit/', $data['ak_id'], '">Editovat</a>';
		echo '<br/>';
		foreach($items as $item) {
			if($active) {
				echo '<input type="radio" name="choice" value="', $item['aki_id'], '" />';
				echo $item['aki_text'];
				echo '<br/>';
			} else {
				echo '<div style="width:60%;position:relative;">', $item['aki_text'],
					'<div style="float:right;">', $item['aki_pocet'], "</div></div>";
				echo '<div style="width:60%;height:10px;line-height:10px;background-color:#D5D5D5;"> ';
				echo '<div style="width:', $item['aki_pocet']/$sum*100, '%;height:10px;background-color:#',
					dechex(rand(40,220)).dechex(rand(40,220)).dechex(rand(40,220)), ';"></div>';
				echo '</div>';
				echo '<div style="height:7px;">&nbsp;</div>';
			}
		}//TODO: /ankety/[0-9]*
		if($active) {
			echo '<button type="submit" name="id" value="', $data['ak_id'], '">Hlasovat</button>';
			echo '<a href="/home/ankety">Zobrazit</a>';
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