<li class="nav-item dropdown">
  <a href="/login" class="nav-link dropdown-toggle dropdown-no-arrow text-white" data-toggle="dropdown">
    <i class="fas fa-user"></i>
    Přihlásit
  </a>
  <ul class="dropdown-menu dropdown-menu-right" role="menu" style="min-width:250px">
    <div class="col-lg-12 pt-2">
      <form action="/login" method="post" role="form">
        <div class="form-group pb-1">
          <input type="text" name="login" id="login" tabindex="1"
                 class="form-control" placeholder="Uživatelské jméno" autofocus autocomplete="username">
        </div>
        <div class="form-group pb-1">
          <input type="password" name="pass" id="pass" tabindex="2"
                 class="form-control" placeholder="Heslo" autocomplete="current-password">
        </div>
        <div class="form-group pb-2">
          <button type="submit" tabindex="3" class="btn btn-primary">
            Přihlásit se
          </button>
        </div>
        <a href="/registrace" tabindex="4">Registrovat se</a><br>
        <a href="/nopassword" tabindex="5">Zapomněli jste heslo?</a><br>
      </form>
    </div>
  </ul>
</li>
