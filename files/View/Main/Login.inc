<div class="container">
  <form action="" method="post">
    <table style="margin: 0 auto;">
      <tr>
        <td class="r">Jméno:</td>
        <td><?= $this->text('login', '')->size(8) ?></td>
      </tr>
      <tr>
        <td class="r">Heslo:</td>
        <td><input class="form-control" type="password" name="pass" size="8" /></td>
      </tr>
      <tr>
        <td class="r"><a href="/registrace">Registrovat se</a></td>
        <td><?= $this->submit('Přihlásit se')->data('action', 'login') ?></td>
      </tr>
      <tr>
        <td colspan="2"><a href="/nopassword">Zapomněli jste heslo?</a></td>
      </tr>
    </table>
  </form>
</div>
