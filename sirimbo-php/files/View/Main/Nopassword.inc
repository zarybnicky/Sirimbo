<div class="container">
  <p>
    Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který jste zadali při registraci.
  </p>

  <form action="" method="post">
    <div class="row m-3">
      <div class="col-12 col-md-6 col-lg-4">
        <div class="form-group">
          <input class="form-control" name="name" placeholder="Uživatelské jméno" />
        </div>
        <div class="form-group">
          <input class="form-control" name="email" placeholder="Email" />
        </div>
        <div class="form-group">
          <?= $this->submit('Získat nové heslo')->data('action', 'gen_pass') ?>
        </div>
      </div>
    </div>
  </form>
</div>
