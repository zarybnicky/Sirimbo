<?php
header_main('Správa plateb');
echo $this->render('files/View/Admin/Platby/OverviewNavigation.inc');
notice($this->redirect()->getRedirectMessage());
?>
<br/>
<?php header_minor('Upload souboru CSV');?>
<form action="" method="POST" enctype="multipart/form-data">
<?php echo $this->upload('in');?>
<button type="submit" name="action" value="upload">Upload</button>
</form>