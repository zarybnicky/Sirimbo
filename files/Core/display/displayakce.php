<?php
class DisplayAkce {
	public static function viewAkceHeader($data) {
		echo '<div class="trenink_header" style="width:330px;">';
		echo '<div class="nadpis">', $data['a_jmeno'], '</div>';
		echo '<div class="nadpis">', $data['a_kde'], '</div>';
		echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($data['a_od']);
		if($data['a_od'] != $data['a_do'])
			echo ' - ', formatDate($data['a_do']);
		echo '</div>';
		echo '<div style="text-align:left;">';
		echo '<span style="color:#572E00;font-size:115%;">Kapacita: </span>',
			$data['a_kapacita'], '<br/>';
		echo '<span style="color:#572E00;font-size:115%;">Volných míst: </span>',
			$data['a_kapacita'] - count(DBAkce::getAkceItems($data['a_id'])), '<br/>';
		echo '</div><br/>';
		
		if(Permissions::check('akce', P_OWNED)) {
			echo '<span style="color:#572E00;font-size:115%;">Admin: </span>';
			echo '<a href="/admin/akce/edit/', $data['a_id'], '">obecné</a>, ';
			echo '<a href="/admin/akce/detail/', $data['a_id'], '">účastníci</a>, ';
			echo '<a href="/admin/akce/dokumenty/', $data['a_id'], '">dokumenty</a>';
		}
	
		echo '<div style="text-align:left;">';
		echo '<span style="color:#572E00;font-size:115%;">Další informace: </span>';
		echo '<br/>', nl2br($data['a_info']);
		echo '</div>';
		$doku = unserialize($data['a_dokumenty']);
		if($doku) {
			echo '<br/>';
			echo '<div style="text-align:left;">';
			echo '<span style="color:#572E00;font-size:115%;">Dokumenty: </span>';
			echo '</span><br/>';
			foreach($doku as $id) {
				$doku_data = DBDokumenty::getSingleDokument($id);
				echo '<a href="/member/download?id=', $id, '">', $doku_data['d_name'], '</a>';
			}
			echo '</div>';
		}
		echo '</div>';
	}
	
	public static function viewFullAkce($id) {
		$akce = DBAkce::getSingleAkce($id, true);
		if(empty($akce)) {
			notice('Neexistuje žádná taková akce');
			return;
		}
		$akce_items = DBAkce::getAkceItems($id);
		
		echo '<div class="trenink_header" style="width:330px;">';
		echo '<div class="nadpis">', $akce['a_jmeno'], '</div>';
		echo '<div style="letter-spacing:1px;font-weight:bold;">', formatDate($akce['a_od']);
		if($akce['a_od'] != $akce['a_do'])
			echo ' - ', formatDate($akce['a_do']), '<br/><br/>';
		echo '<span style="color:#572E00;font-weight:normal;font-size:115%;">Kapacita: </span>',
			$akce['a_kapacita'], '<br/>';
		echo '<span style="color:#572E00;font-weight:normal;font-size:115%;">Volných míst: </span>',
			$akce['a_kapacita'] - count($akce_items), '<br/>';
		echo '</div>';
		
		if(Permissions::check('akce', P_OWNED)) {
			echo '<span style="color:#572E00;font-size:115%;">Admin: </span>';
			echo '<a href="/admin/akce/edit/', $akce['a_id'], '">obecné</a>, ';
			echo '<a href="/admin/akce/detail/', $akce['a_id'], '">účastníci</a>, ';
			echo '<a href="/admin/akce/dokumenty/', $akce['a_id'], '">dokumenty</a>';
		}
		echo '</div>';
		
		echo '<div style="text-align:center;">';
		if(empty($akce_items)) {
			echo 'Nikdo není přihlášený<br/>';
		} else {
			echo '<table style="margin:0 auto;">';
			echo '<tr><td>Jméno</td><td>Rok narození</td></tr>';
			foreach($akce_items as $par) {
				echo '<tr><td>', $par['u_jmeno'], ' ', $par['u_prijmeni'], '</td><td>',
				$par['ai_rok_narozeni'], '</td></tr>';
			}
			echo '</table>';
		}
		echo '<a href="/member/akce">Zpět</a>';
		echo '</div>';
	}
}