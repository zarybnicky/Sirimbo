<h1>
    <?= $this->header ?>
</h1>
<form action="" method="POST">
	<?= $this->prompt ?>
	<br/>
	<br/>
	<?php foreach ($data as $item) : ?>
	   <?= $item['text'] ?>
	    <input type="hidden" name="data[]" value="<?= $item['id'] ?>" />
	    <br />
	<?php endforeach; ?>
	<br/>
	<button type="submit" name="action" value="confirm">
	    Odstranit
	</button>
	<a href="<?= $this->returnUrl ?>">
	   Zpět
    </a>
</form>