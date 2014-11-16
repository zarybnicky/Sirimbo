<?= $this->render('files/View/Admin/Platby/Navigation.inc') ?>
<?= $this->subheader('Import plateb') ?>

<div class="container full">
<?php
echo '<h3>Upload souboru CSV</h3>';
?>
<form action="" method="POST" enctype="multipart/form-data">
<?php echo $this->upload('in');?>
<button type="submit" name="action" value="upload">Upload</button>
</form>
</div>
