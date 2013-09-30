<h1>Profil</h1>
<?php
echo $this->partnerRequest()->getAll();
notice($this->redirect()->getMessage());
if($this->havePartner) {
	echo "Právě tančím s: ", $this->partnerFullName;
} else {
	echo 'Právě s nikým netančím';
}
?>
<form method="POST" action="">
<?php echo $this->userSelect()
	->name('partner')->tmpSwitch(false)
	->users($this->users);?>
<button type="submit" name="action" value="confirm">Požádat o partnerství</button>
<?php if($this->havePartner): ?>
	<button type="submit" name="action" value="dumpthem">Rozejít se</button>
<?php endif;?>
<a href="/member/profil/par">Zpět</a>
</form>