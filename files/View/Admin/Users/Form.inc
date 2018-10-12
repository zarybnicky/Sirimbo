<div class="container">
  <form action="" method="POST">
    <div class="form-group">
      <label for="login">Přihlašovací jméno</label>
      <?= $this->text('login', $this->login)->readonly($this->action == 'edit') ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků
      </small>
    </div>
    <div class="form-group">
      <label for="pass">Heslo</label>
      <?= $this->text('pass', $this->pass)->disabled($this->action == 'edit') ?>
      <small class="form-text text-muted">
        Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků
      </small>
    </div>
    <div class="form-group">
      <label for="jmeno">Jméno</label>
      <?= $this->text('jmeno', $this->jmeno) ?>
      <small class="form-text text-muted">Max. 40 znaků</small>
    </div>
    <div class="form-group">
      <label for="prijmeni">Příjmení</label>
      <?= $this->text('prijmeni', $this->prijmeni) ?>
      <small class="form-text text-muted">Max. 40 znaků</small>
    </div>
    <div class="form-group">
      <label for="pohlavi">Pohlaví</label>
      <div class="form-check form-check-inline">
        <?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm')->label('Muž') ?>
      </div>
      <div class="form-check form-check-inline">
        <?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f')->label('Žena') ?>
      </div>
    </div>
    <div class="form-group">
      <label for="email">E-mail</label>
      <?= $this->text('email', $this->email) ?>
      <small class="form-text text-muted">Max. 50 znaků</small>
    </div>
    <div class="form-group">
      <label for="telefon">Telefon</label>
      <?= $this->text('telefon', $this->telefon) ?>
      <small class="form-text text-muted">Pouze číslice a +, max. 13 znaků</small>
    </div>
    <div class="form-group">
      <label for="od">Datum narození</label>
      <?= $this->date('narozeni', $this->narozeni) ?>
    </div>
    <div class="form-group">
      <label for="poznamky">Poznámky</label>
      <textarea class="form-control" name="poznamky" rows="15"><?= $this->poznamky ?></textarea>
    </div>
    <div class="form-check">
      <?= $this->checkbox('lock', 'lock')->label('Uzamknout')->set($this->lock) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('ban', 'ban')->label('Ban')->set($this->ban) ?>
    </div>
    <div class="form-check">
      <?= $this->checkbox('system', 'system')->label('Systémový uživatel')->set($this->system) ?>
    </div>
    <div class="form-group">
      <label for="group">Oprávnění</label>
      <?php
      $s = $this->select('group')->set($this->group);
      foreach ($this->groups as $item) {
          $s->option($item['id'], $item['name']);
      }
      echo $s;
      ?>
    </div>
    <div class="form-group">
      <div style="font-weight:bold">Skupiny</div>
      <?php
      foreach ($this->skupiny as $item) {
          echo $this->radio('skupina', $item['id'])
                   ->set($this->skupina == $item['id'])
                   ->label($this->colorbox($item['color'], $item['popis'])
                        . '&nbsp;' . $item['popis']);
          echo '<br/>';
      }?>
    </div>
    <div class="form-group">
      <a class="btn btn-outline-primary" href="/admin/users">Zpět</a>
      <?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
    </div>
  </form>
</div>
