<?php
echo $this->render('files/View/Admin/Platby/Navigation.inc');
echo '<h2>Import plateb</h2>';
echo '<h3>Upload souboru CSV</h3>';
?>
<form action="" method="POST" enctype="multipart/form-data">
<?php echo $this->upload('in');?>
<button type="submit" name="action" value="upload">Upload</button>
</form>