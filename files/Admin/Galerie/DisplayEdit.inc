<h1>Správa fotogalerie</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<a href="/admin/galerie/upload">Přidat fotky</a>
<button type="submit" name="action" value="edit">Upravit fotku</button>
<button type="submit" name="action" value="remove">Odstranit fotky</button>
<?php
if(!($data = DBGalerie::getSingleDir($id))) {
    $this->redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
}
$fotos = DBGalerie::getFotky($id);
$count = count($fotos);

if($count < 1) {
    notice('Žádné fotky');
} else {
    echo '<div>';
    foreach($fotos as $item) {
        $tn = str_replace('./galerie', '/galerie/thumbnails', $item['gf_path']);
        
        echo '<div class="f">';
        echo '<div class="f_img"><div class="f_preview">';
        echo '<img alt="', $item['gf_id'], '" src="', $tn, '" />';
        echo '</div></div>';
        echo '<div class="f_popis"><input type="checkbox" name="galerie[]" value="' . $item['gf_id'] . '" />',
            $item['gf_name'], '</div>';
        echo '</div>';
    }
    echo '</div>';
}
?>
</form>
<a href="/admin/galerie" style="clear:both;display:block;">Zpět</a>