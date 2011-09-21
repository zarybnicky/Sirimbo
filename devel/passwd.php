<?php
require("../files/Core/user.php");

if(!empty($_POST["passwd"]))
	echo "Enciphered passwd: ", User::Crypt($_POST["passwd"]), "<br/><br/>\n";
?>
<form method="POST" action="/devel/passwd.php">
<input type="text" value="" name="passwd" />
<button type="submit">Encrypt</button>
</form>