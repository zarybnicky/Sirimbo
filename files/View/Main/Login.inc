<div class="container full">
<?php echo $this->notice($this->redirect()->getMessage()); ?>
<br/>
<form action="" method="post">
<table style="margin: 0 auto;">
<tr><td class="r">Jméno:</td><td><input type="text" name="login" size="8" /></td></tr>
<tr><td class="r">Heslo:</td><td><input type="password" name="pass" size="8" /></td></tr>
<tr>
    <td class="r"><a href="/registrace">Registrovat se</a></td>
    <td><button type="submit" name="action" value="login">Přihlásit se</button></td>
</tr>
<tr><td colspan="2"><a href="/nopassword">Zapomněli jste heslo?</a></td></tr>
</table>
</form>
</div>
