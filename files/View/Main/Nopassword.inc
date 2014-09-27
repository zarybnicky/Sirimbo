<?php echo $this->notice($this->redirect()->getMessage());?>

<p style="text-align:center;">Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který
jste zadali při registraci</p>

<form action="" method="post">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<table style="margin:0 auto;">
<tr>
    <td>Uživatelské jméno:</td>
    <td><input type="text" name="name" /></td>
</tr>
<tr>
    <td>Email:</td>
    <td><input type="text" name="email" /></td>
</tr>
<tr>
    <td></td>
    <td><button type="submit" name="action" value="gen_pass">Generovat heslo</button></td>
</tr>
</table>
</form>
