<form action="<?php echo $_SERVER["REQUEST_URI"] ?>" method="POST">
<?php
if($id == 0)
	View::redirect('/admin/galerie', 'Není možné upravit hlavní složku');
if(!($data = DBGalerie::getSingleDir($id)))
	View::redirect('/admin/galerie', 'Složka s takovým ID neexistuje');

$f = new MenuHelper();
echo $f->menu()
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
		<td>
			<select name="parent">
				<option value="none" selected="selected">Vyber si :o)</option>
				<?php
				$dirs = DBGalerie::getDirs(true, true);
				foreach($dirs as $dir) {
					if($dir['gd_id'] == $data['gd_id'])
						continue;
					echo '<option value="', $dir['gd_id'], '-', $dir['gd_level'], '"';
					if($data['gd_id_rodic'] == $dir['gd_id'])
						echo ' selected="selected"';
					echo '>',
						str_repeat("&nbsp;-&nbsp;", $dir['gd_level'] - 1), $dir['gd_name'], '</option>';
				}
				?>
			</select>
		</td>
	</tr><tr>
		<td>Skrytá složka: </td>
		<td><input type="checkbox" name="hidden"
			<?php echo $data['gd_hidden'] == '1' ? 'checked="checked"' : ''; ?> />
	</tr>
</table>
</form>