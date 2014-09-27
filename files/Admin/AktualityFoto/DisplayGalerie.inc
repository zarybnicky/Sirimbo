<h1>Správa fotogalerie článků</h1>
<?php
echo $this->notice($this->redirect()->getMessage());

echo '<form action="" method="get">';
$s = $this->select()
    ->get()->name('dir')->value('none')
    ->option('none', 'Vybrané fotky')
    ->option('no', '-------------');

$dirs = DBGalerie::getDirs(true, true);
foreach ($dirs as $dir) {
    $s->option($dir['gd_id'], str_repeat("&nbsp;&nbsp;", $dir['gd_level'] - 1)
                                        . $dir['gd_name']);
}
echo $s;
echo '<button type="submit">Zobrazit</button>';
echo '</form>';

echo '<form action="" method="post">';
echo '<button type="submit" name="action" value="enter" style="display:none;">Enter</button>';

$id = get('dir');
$items = DBGalerie::getFotky($id);
if (empty($items)) {
    echo $this->notice('Žádné fotky');
    return;
}
foreach ($items as $item) {
    echo $item['gf_path'], '<br/>';
}
echo '</form>';
