<div class="container">
<h1>Správa článků</h1>

<?php echo $this->notice($this->redirect()->getMessage());

echo '<form action="" method="get">';
echo $this->select()
          ->get()->name('dir')->value('none')
          ->option('none', '------ vyberte složku ------')
          ->options($this->dirs);
echo '<button type="submit">Zobrazit</button>';
echo '</form>';

echo '<form action="" method="post">';

foreach ($this->photos as $item) {
    echo '<div class="foto_preview">';
    echo '<div class="foto_preview_in">';
    echo '<img alt="' . $item['id'] . '" src="' . $item['src'] . '" />';
    echo '</div>';
    echo '<div class="foto_desc">';
    echo $this->radio('foto', $item['id']);
    echo '<br/><br/></div>';
    echo '</div>';
}
echo '<div style="clear:both">';
echo $this->submit('Potvrdit');
echo '<a href="/admin/aktuality">Zpět</a>';
echo '</div>';
echo '</form>';
echo '</div>';
