<?php
header_main('Inzerce');

if(!empty($_POST)) {
	
}

$result = DisplayInzerce::viewInzerce(true, true);
if(!$result)  {
	notice('Žádné inzerce');
	return;
}
?>