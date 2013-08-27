<?php header_main('Správa plateb'); ?>
<div style="float:right;width:350px;margin-top:-5px;">
	<?php if($this->currentUser != array()): ?>
	<div>
		<?php header_minor('Aktuální uživatel');?>
		<table style="clear:right;">
			<tr>
				<td>Jméno</td>
				<td><?php echo $this->currentUser['fullName'];?></td>
			</tr><tr>
				<td>Skupina</td>
				<td><?php echo $this->currentUser['colorBox'];?></td>
			</tr>
		</table>
	</div>
	<?php endif; ?>
	
	<div style="clear:right;">
		<?php header_minor('Přehled skupin');
		echo $this->table()
			->style('width:100%;clear:right;')
			->data($this->skupiny)
			->column('description', 'Skupina')
			->column('platbaMesic', 'Kč/<br/>měsíc', 'center')
			->column('platbaCtvrtleti', 'Kč/<br/>čtvrtletí', 'center')
			->column('platbaPololeti', 'Kč/<br/>pololetí', 'center');
		?>
	</div>
</div>

<?php
if($this->action == 'add')
	header_minor("Přidat platbu");
else
	header_minor("Upravit platbu");
notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST">
<table>
	<tr>
		<td>Uživatel (*): </td>
		<td><?php
		$s = $this->select()
			->post()->name('user');
		foreach($this->users as $user)
			$s->option($user['u_id'], $user['u_prijmeni'] . ', ' . $user['u_jmeno']);
		echo $s;
		?>
		</td>
	</tr><tr>
		<td>Částka (*): </td>
		<td><input type="text" name="castka" value="<?php echo post('castka'); ?>" /></td>
	</tr><tr>
		<td>Placeno: (*)</td>
		<td><?php echo $this->date('placeno')->textBox();?>	</td>
	</tr><tr>
		<td>Platební období (*): </td>
		<td><?php
		$s = $this->select();
		$s->post()->name('obdobi');
		foreach(Settings::$platby_obdobi as $key => $value)
			$s->option($key, $value[2]);
		echo $s;
		?>
		<input type="hidden" name="referer" value="<?php echo $this->returnURL?>" />
		</td>
	</tr><tr>
		<td><button type="submit"><?php
			echo ($this->action == 'add' ? 'Přidat' : 'Upravit');
		?></button></td>
		<td>Pole označená (*) jsou povinná</td>
	</tr>
</table>
</form>
<a href="<?php echo $this->returnURL; ?>">Zpět</a>