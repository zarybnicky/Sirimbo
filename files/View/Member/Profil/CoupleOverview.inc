<?php
header_main('Profil');
echo $this->partnerRequest()->getAll();
notice($this->redirect()->getMessage());
?>
<?php if($this->havePartner): ?>
	Právě tančím s: <?php echo $this->partnerFullName;?>
	<?php header_minor('Třídy a body:');?>
	
	Standardní tance:
	<br />
	Třída: <span style="font-weight:bolder;"><?php echo $this->sttTrida;?></span>,
	body: <?php echo $this->sttBody;?>,
	finále: <?php echo $this->sttFinale;?>
	<br />
	Latinsko-americké tance:
	<br />
	Třída: <span style="font-weight:bolder;"><?php echo $this->latTrida;?></span>,
	body: <?php echo $this->latBody;?>,
	finále: <?php echo $this->latFinale;?>
	<br /><br/>
	V "Olympáckém žebříčku" máme <?php echo $this->hodnoceni;?> bodů.
	<br />
	<a href="/member/profil/par/body">Změnit třídu a body</a>
<?php else: ?>
	Právě s nikým netančím.<br/>
<?php endif;?>
<a href="/member/profil/par/partner">Změnit partnera</a>
<a href="/member/profil">Zpět</a>