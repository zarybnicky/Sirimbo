<?php header_main('Klubové akce');?>
<?php notice($this->redirect()->getMessage());?>

<style type="text/css">
.unit {background:inherit;border:none;
vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;}
.unit form {text-align:center;}
</style>
<table style="width:100%;">
	<?php foreach(array_chunk($this->akce, 2) as $chunk): ?>
	<tr>
		<?php foreach($chunk as $data):?>
		<td class="unit">
			<form action="" method="post">
			<?php echo $this->render('files/View/Member/Akce/Header.inc', $data);?>
			
			<?php if($data['showForm']): ?>
				<input type="hidden" name="id" value="<?php echo $data['id'];?>" />
				
				<?php if($data['signIn']): ?>
				<button type="submit" name="action" value="signup">Přihlásit se</button> &bull; 
				<?php else: ?>
				<button type="submit" name="action" value="signout">Odhlásit se</button> &bull; 
				<?php endif; ?>
			<?php endif; ?>
			
			<a href="/member/akce/<?php echo $data['id'];?>">Zobrazit přihlášené</a>
			</form>
		</td>
		<?php endforeach;?>
	</tr>
	<?php endforeach;?>
</table>