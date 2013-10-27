<h1>Správa fotogalerie</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<?php
if ($id == 0)
    $this->redirect('/admin/galerie', 'Není možné upravit hlavní složku');
if (!($data = DBGalerie::getSingleDir($id)))
    $this->redirect('/admin/galerie', 'Složka s takovým ID neexistuje');

echo $this->menu()
    ->float(MenuHelper::FLOAT_RIGHT)
    ->content('Uložit', 'save', true)
    ->content('Zpět', '/admin/galerie');
?>
<table>
    <tr>
        <td>Název: </td>
        <td><input type="text" name="name" value="<?php echo $data['gd_name']; ?>" />
    </tr><tr>
        <td>Nadsložka</td>
        <td><?php
            $dirs = DBGalerie::getDirs(true, true);
            $s = $this->select()
                ->post()->name('parent')
                ->value($data['gd_id_rodic'])
                ->option('none', '--');
            foreach ($dirs as $dir) {
                if ($dir['gd_id'] == $data['gd_id'])
                    continue;
                $s->option($dir['gd_id'] . '-' . $dir['gd_level'],
                    str_repeat("&nbsp;&nbsp;", $dir['gd_level'] - 1) . $dir['gd_name']);
            }
            echo $s;
            ?>
        </td>
    </tr><tr>
        <td>Skrytá složka: </td>
        <td><label><?php echo getCheckbox('hidden', '1', false, false, $data['gd_hidden'])?></label>
    </tr>
</table>
</form>