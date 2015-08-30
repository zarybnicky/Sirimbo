<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <br/>
    <form action="" method="post">
        <table style="margin: 0 auto;">
            <tr>
                <td class="r">Jméno:</td>
                <td><input type="text" name="login" size="8" /></td>
            </tr>
            <tr>
                <td class="r">Heslo:</td>
                <td><input type="password" name="pass" size="8" /></td>
            </tr>
            <tr>
                <td class="r">
                    <a href="/registrace">Registrovat se</a>
                </td>
                <td>
                    <?= $this->submit('Přihlásit se')->data('action', 'login') ?>
                </td>
            </tr>
            <tr>
                <td colspan="2">
                    <a href="/nopassword">Zapomněli jste heslo?</a>
                </td>
            </tr>
        </table>
    </form>
</div>
