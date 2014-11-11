<?= $this->header('Správa uživatelů') ?>
<div class="container full">
<h2>Platby uživatele <?php echo $this->fullName;?></h2>
<?php
echo $this->notice($this->redirect()->getMessage());
echo $this->notice('Ještě není převedeno do nové verze plateb :(');
?>
</div>
