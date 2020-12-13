<?= new \RenderHelper('files/View/Admin/Platby/Navigation.inc', ['uri' => $this->uri]) ?>

<div class="container">
    <h3>Upload souboru CSV</h3>
    <form action="" method="POST" enctype="multipart/form-data">
        <?= new \UploadHelper('in') ?>
        <?= $this->submit('Nahrát')->data('action', 'upload') ?>
    </form>
</div>
