<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
    <h3>Upload souboru CSV</h3>
    <form action="" method="POST" enctype="multipart/form-data">
        <?= new \UploadHelper('in') ?>
        <?= \Utils::submit('Nahrát', 'action', 'upload') ?>
    </form>
</div>
