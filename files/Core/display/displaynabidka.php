<?php
class DisplayNabidka {
	public static function viewNabidkaHeader($data, $obsazeno) {
		echo '<div class="trenink_header">';
		echo '<div class="nadpis">';
		echoFullJmeno($data);
		echo '</div>';
		
		if(Permissions::canEditNabidka($data['n_trener']))
			echo '<a href="/admin/nabidka/edit/', $data['n_id'], '">Editovat</a>';
		
		echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($data['n_od']);
		if($data['n_od'] != $data['n_do'])
			echo ' - ', formatDate($data['n_do']);
		echo '</b></div>';
		echo '<div><span class="little">Celkem hodin: </span>',
			'<span class="nadpis">', $data['n_pocet_hod'], '</span></div>';
		echo '<div><span class="little">Obsazených hodin: </span>',
			'<span class="nadpis">', (int) $obsazeno, '</span></div>';
		echo '<div><span class="little">Volných hodin: </span>',
			'<span class="nadpis">', $data['n_pocet_hod'] - (int) $obsazeno, '</span></div>';
		echo '</div>';
	}
}