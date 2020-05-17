<div class="container">
  <form action="" method="post">
    <div class="row justify-content-center m-3">
      <div class="col-12 col-md-6 col-lg-4">
        <div class="form-group pb-1">
          <input class="form-control" name="login" placeholder="Uživatelské jméno" />
        </div>
        <div class="form-group">
          <input class="form-control" name="pass" placeholder="Heslo" type="password" />
        </div>
        <div class="form-group pb-2">
          <?= $this->submit('Přihlásit se')->data('action', 'login') ?>
        </div>
        <a href="/registrace">Registrovat se</a><br>
        <a href="/nopassword">Zapomněli jste heslo?</a>
      </div>
    </div>
  </form>
</div>
