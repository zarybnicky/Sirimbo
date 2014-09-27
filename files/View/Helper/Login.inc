<form action="<?php Request::getURI();?>" method="post">
  <div id="userbox">
    <div id="userbox-inner">
      <input style="width:70px;" type="text" name="login" />
      <input style="width:70px;" type="password" name="pass" /><br />
      <a href="/registrace">Registrace</a>&nbsp;
      <a href="javascript:void(0)"
         onclick="$(this).parent().parent().parent().submit()">
        <!-- FIXME: Form submit -->
        Přihlásit se
      </a>
    </div>
  </div>
</form>
