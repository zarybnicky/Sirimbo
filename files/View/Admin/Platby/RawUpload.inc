<?= $this->render('files/View/Admin/Platby/Navigation.inc', array('uri' => $this->uri)) ?>
<?= $this->subheader('Import plateb') ?>

<div class="container full">
    <h3>Upload souboru CSV</h3>
    <form action="" method="POST" enctype="multipart/form-data">
        <?= $this->upload('in') ?>
        <?= $this->submit('Nahrát')->data('action', 'upload') ?>
    </form>
</div>
