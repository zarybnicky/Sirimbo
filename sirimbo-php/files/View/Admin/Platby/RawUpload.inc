<?php new \RenderHelper('files/View/Admin/Platby/Navigation.inc', [], true) ?>

<div class="container">
    <h3>Upload souboru CSV</h3>
    <form action="" method="POST" enctype="multipart/form-data">
        <?= new \UploadHelper('in') ?>
        <?= (new \SubmitHelper('Nahrát'))->data('action', 'upload') ?>
    </form>
</div>
