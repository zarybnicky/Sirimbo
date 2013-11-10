<form action="/ankety" method="POST">
<b><?php echo $this->text;?></b><br/>
<?php foreach ($this->items as $item): ?>
    <input type="radio" name="choice" value="<?php echo $item['id'];?>" /><?php echo $item['text'];?><br/>
<?php endforeach;?>

<button type="submit" name="id" value="<?php echo $this->id;?>">Hlasovat</button>
<a href="/home/ankety">Zobrazit</a>
</form>