<?php
class DisplayNabidka {
	public static function viewNabidkaHeader($data, $obsazeno) {
		echo '<div class="trenink_header">';
		echo '<div class="nadpis">';
		echoFullJmeno($data);
		echo '</div>';
		echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($data['n_od']);
		if($data['n_od'] != $data['n_do'])
			echo ' - ', formatDate($data['n_do']);
		echo '</div>';
		
		if(Permissions::canEditNabidka($data['n_trener'])) {
			echo '<span class="big">Admin:</span> ';
			echo '<a href="/admin/nabidka/edit/', $data['n_id'], '">obecné</a>, ';
			echo '<a href="/admin/nabidka/detail/', $data['n_id'], '">tréninky</a>';
		}
		
		echo '<div><span class="little">Celkem hodin: </span>',
			'<span class="nadpis">', $data['n_pocet_hod'], '</span></div>';
		echo '<div><span class="little">Obsazených hodin: </span>',
			'<span class="nadpis">', (int) $obsazeno, '</span></div>';
		echo '<div><span class="little">Volných hodin: </span>',
			'<span class="nadpis">', $data['n_pocet_hod'] - (int) $obsazeno, '</span></div>';
		echo '</div>';
	}
}