<div class="container">
  <form action="" method="post">
    <div class="row justify-content-center m-3">
      <div class="col-12 col-md-6 col-lg-4">
        <div class="form-group">
          <input class="form-control" name="login" placeholder="Jméno" />
        </div>
        <div class="form-group">
          <input class="form-control" name="pass" placeholder="Heslo" type="password" />
        </div>
        <div class="form-group">
          <?= $this->submit('Přihlásit se')->data('action', 'login') ?>
          <a class="btn btn-outline-primary" href="/registrace">Registrovat se</a>
        </div>
        <a href="/nopassword">Zapomněli jste heslo?</a>
      </div>
    </div>
  </form>
</div>
