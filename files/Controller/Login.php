<?php
class Controller_Login extends Controller_Abstract {
    function view($id = null) {
		if(User::isLogged()) {
			if(get('return'))
				View::redirect(get('return'));
			else
				View::redirect('/member/home');
		}
        
        notice(View::getRedirectMessage());
        ?>
        <style>td.r {text-align:right;}</style>
        
        <br/>
        <form action="<?php echo $_SERVER['REQUEST_URI'] ?>" method="post">
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
        <?php
    }
    function sidebar() {

	}
}
?>