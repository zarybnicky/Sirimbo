<?php
header_main('Správa fotogalerie článků');
notice(View::getRedirectMessage());

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

$id = get('dir');
$items = DBGalerie::getFotky($id);
if(empty($items)) {
	notice('Žádné fotky');
	return;
}
foreach($items as $item) {
	echo $item['gf_path'], '<br/>';
}
echo '</form>';
?>