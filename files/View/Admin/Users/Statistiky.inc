<?php
header_main('Správa uživatelů');
header_minor('Statistiky');
?>
<br/>
<div class="center">
<?php
echo $this->table()
	->style('margin:0 auto;')
	->column('group', '', 'r')
	->column('count', 'Počet', 'center big')
	->data($this->data);
?>
<br/>
<a href="/admin/users">Zpět</a>
</div>