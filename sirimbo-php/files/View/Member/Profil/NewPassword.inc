<div class="container">
  <form action="" method="post">
    <table>
      <tr>
        <td>Staré heslo:</td>
        <td><input type="password" name="oldpass" /></td>
      </tr>
      <tr>
        <td>Nové heslo:</td>
        <td><input type="password" name="newpass" /></td>
      </tr>
      <tr>
        <td>Znovu nové heslo:</td>
        <td><input type="password" name="newpass_confirm" /></td>
      </tr>
      <tr>
        <td><?= new \SubmitHelper('Změnit heslo') ?></td>
        <td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
      </tr>
    </table>
  </form>
  <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
</div>
