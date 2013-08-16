<?php header_main("Členové");?>

<?php if($this->showMenu): ?>
	<form action="" method="get">
	Zobrazení:&nbsp;
	<?php echo $this->select()
		->get()->name('f')
		->option('skupiny', 'podle skupin')
		->option('dancer', 'tanečníci')
		->option('all', 'všichni');?>
	<button type="submit">Odeslat</button>
	</form>
<?php endif;?>

<?php if($this->viewType == 'skupiny'): ?>
	<table style="width:100%;border:none;background:inherit;">
	<tr style="border:none;background:inherit;">
	<?php foreach($this->data as $row): ?>
		<td style="border:none;background:inherit;vertical-align:top;">
		<?php foreach($row as $skupina): ?>
			<?php header_minor($skupina['header']);?>
			
			<?php if(!empty($skupina['users'])): ?>
				<table style="width:100%">
				<tr class="little">
					<td>Jméno</td><td style="width:55px;">Zaplaceno</td><td>Platí do</td>
				</tr>
				<?php foreach($skupina['users'] as $user): ?>
					<tr>
					<td>
						<a href="/member/clenove/<?php echo $user['id'];?>">
							<img src="/style/person-small.png" alt="" style="margin-bottom:-2px"/>
						</a>&nbsp;
						<?php echo $user['fullName'];?>
					</td>
					<td style="width:25px;">
					<?php if($user['zaplaceno']): ?>
						<span style="color:#0B0;">ANO</span>
					<?php else: ?>
						<span style="color:#B00;font-weight:bold;">NE</span>
					</td>
					<?php endif;?>
					
					<td style="width:75px;"><?php echo $user['platbaDatum'];?></td>
					</tr>
				<?php endforeach;?>
				</table>
			<?php endif;?>
			<br/>
		<?php endforeach;?>
		</td>
	<?php endforeach;?>
	</table>
<?php else: ?>
	<br/>
	Celkem:&nbsp;<span class="big"><?php echo $this->count;?></span><br/>
	<table>
	<?php $i = 0; foreach($this->users as $user): ?>
		<tr>
		<td>&nbsp;<?php echo ++$i;?>.</td>
		<td>
			<a href="/member/clenove/<?php echo $user['id'];?>">
			<img src="/style/person-small.png" alt="" style="margin-bottom:-2px"/></a>&nbsp;
			<?php echo $user['fullName'];?>
		</td>
		<?php if($this->showGroup): ?>
			<td>
			<?php echo $user['groupDescription'];?>
			</td>
		<?php endif;?>
		</tr>
	<?php endforeach;?>
	</table>
	<br/>
<?php endif;?>