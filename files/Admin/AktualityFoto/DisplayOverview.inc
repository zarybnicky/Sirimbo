<?php
$dirs = DBGalerie::getDirs(true, true);

echo '<form action="', Request::getURI(), '" method="get">';
$s = Helper::get()->select()
	->get()->name('dir')->value('none')
	->option('none', 'Vybrané fotky')
	->option('no', '-------------');
foreach($dirs as $dir)
	$s->option($dir['gd_id'], str_repeat("&nbsp;&nbsp;", $dir['gd_level'] - 1) . $dir['gd_name']);
echo $s;
echo '<button type="submit">Zobrazit</button>';
echo '</form>';

echo '<form action="', Request::getURI(), '" method="post">';
echo '<button type="submit" name="action" value="enter" style="display:none;">Enter</button>';

dumpVar($_POST);

$items[] = array('af_id' => 1, 'af_path' => './galerie/xichty treneru/jerry.jpg');
$items[] = array('af_id' => 2, 'af_path' => './galerie/xichty treneru/jerry.jpg');
$items[] = array('af_id' => 3, 'af_path' => './galerie/xichty treneru/jerry.jpg');

DisplayAktuality::viewClanek($data, true);
if(empty($items)) {
	notice('Žádné vybrané fotky');
	return;
}
foreach($items as $item) {
	echo '<div class="float" style="float:left;">';
	echo '<div class="image">';
	echo '<img src="', str_replace('./galerie', '/galerie/thumbnails', $item['af_path']), '" alt="" />';
	echo '</div>';
	echo '<label>', getRadio('hlavni', $item['af_id'], $data['at_foto_main']), ' Hlavní</label>';
	echo '</div>';
}
echo '<div style="clear:both;"></div>'
?>
<button type="submit" name="action" value="submit">Uložit</button>
<a href="/admin/akce">Zpět</a>
</form>