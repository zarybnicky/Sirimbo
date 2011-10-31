<?php
class DisplayAkce {
	public static function viewAkceHeader($data) {
		echo '<div class="a_info" style="border:1px solid black;">';
		echo '<div class="a_jmeno">Název: ', $data['a_jmeno'], '</div>';
		echo '<div class="a_datum">Datum: ', formatDate($data['a_od']);
		if($data['a_od'] != $data['a_do'])
			echo ' - ', formatDate($data['a_do']);
		echo '</div>';
		echo '<div class="a_moreinfo">Další informace: ', $data['a_info'], '</div>';
		echo '<div class="a_dokumenty">Dokumenty:<br/>';
		$doku = unserialize($data['a_dokumenty']);
		if($doku) {
			foreach($doku as $id) {
				$doku_data = DBDokumenty::getSingleDokument($id);
				echo '<a href="/member/download?id=', $id, '">', $doku_data['d_name'], '</a>';
			}
		} else {
			echo 'Žádné dokumenty';
		}
		echo '</div>';
		echo '</div>';
	}
	
	public static function viewFullAkce($id) {
		$akce = DBAkce::getSingleAkce($id);
		if(empty($akce)) {
			notice('Neexistuje žádná taková akce');
			return;
		}
		$akce_items = DBTas::getAkceItems($id);
		
		echo '<div class="a_wrapper">';
		
		DisplayAkce::viewAkceHeader($akce);
		
		if(empty($akce_items)) {
			echo 'Nikdo není přihlášený';
		} else {
			echo '<table>';
			echo '<tr><td>Jméno</td><td>Příjmení</td><td>Rok narození</td></tr>';
			foreach($akce_items as $par) {
				echo '<tr><td>', $par['ai_jmeno'], '</td><td>', $par['ai_prijmeni'], '</td><td>',
				$par['ai_rok_narozeni'], '</td></tr>';
			}
			echo '</table>';
		}
		echo '</div>';
		echo '<a href="/member/akce">Zpět</a>';
	}
}