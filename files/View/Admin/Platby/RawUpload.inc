<?php
echo $this->render('files/View/Admin/Platby/OverviewNavigation.inc');
header_main('Import plateb');
header_minor('Upload souboru CSV');
?>
<form action="" method="POST" enctype="multipart/form-data">
<?php echo $this->upload('in');?>
<button type="submit" name="action" value="upload">Upload</button>
</form>