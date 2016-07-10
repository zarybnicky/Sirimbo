<?php
class UserSelectHelper
{
    private $_name;
    private $_idIndex;
    private $_jmeno;
    private $_prijmeni;
    private $_users;
    private $_type;
    private $_tmpSwitch;
    private $_selected;

    public function __construct() {
        return $this->userSelect();
    }

    public function userSelect($name = null) {
        $this->_defaultValues();
        if ($name !== null)
            return $this->name($name);
        return $this;
    }
    private function _defaultValues() {
        $this->_name = "user";
        $this->_idIndex = "u_id";
        $this->_jmeno = "u_jmeno";
        $this->_prijmeni = "u_prijmeni";
        $this->_users = array();
        $this->_type = 'user';
        $this->_tmpSwitch = true;
    }
    public function name($name) {
        $this->_name = $name;
        return $this;
    }
    public function idVar($idVar) {
        $this->_idIndex = $idVar;
        return $this;
    }
    public function jmeno($jmeno) {
        $this->_jmeno = $jmeno;
        return $this;
    }
    public function prijmeni($prijmeni) {
        $this->_prijmeni = $prijmeni;
        return $this;
    }
    public function users(array $users) {
        if (is_array($users))
            $this->_users = $users;
        return $this;
    }
    public function type($type) {
        if ($type == 'user' || $type == 'par')
            $this->_type = $type;
        return $this;
    }
    public function tmpSwitch($value) {
        $this->_tmpSwitch = (bool) $value;
        return $this;
    }
    public function set($value) {
        $this->_selected = $value;
        return $this;
    }
    public function render() {
        $name = 'userselect' . rand(0, 1024);
        $selected = $this->_selected !== null ? $this->_selected : '';

        $out = '<div class="' . $name . '">' . "\n";
        $out .= '<select name="' . $this->_name . '">' . "\n";
        if (!$selected) {
            $out .= '<option value="0" selected="selected">--- žádný ---</option>' . "\n";
        } else {
            $out .= '<option value="0">--- žádný ---</option>' . "\n";
        }

        if ($this->_tmpSwitch) {
            $out .= '<option value="temporary">--- dočasný ---</option>' . "\n";
        }

        foreach ($this->_users as $user) {
            if (isset($user['u_narozeni'])) {
                list($year, $month, $day) = explode('-', $user['u_narozeni']);
            }

            $id = $user[$this->_idIndex];
            if ($id == $selected)
                $out .= '<option value="' . $id . '" selected="selected">';
            else
                $out .= '<option value="' . $id . '">';
            $out .= $user[$this->_prijmeni] . ', ' . $user[$this->_jmeno] .
                (isset($user['u_narozeni']) ? (', ' . $year) : '');
            $out .= '</option>' . "\n";
        }
        $out .= '</select>' . "\n";

        if ($this->_tmpSwitch) {
            ob_start();
        ?>
<noscript><br/><a href="/admin/users/temporary">Nový dočasný uživatel</a></noscript>
<div class="new" style="display:none;text-align:right;">
Jméno: <input type="text" name="jmeno" size="8" /><br/>
Příjmení: <input type="text" name="prijmeni" size="8" /><br/>
Datum narození:&nbsp;<br/>
<?php echo Helper::instance()->date('narozeni')->render(), '<br/>';?>
<button type="submit">Uložit</button>
</div>
<div class="loading" style="display:none;"><img alt="Čekám na odezvu serveru..." src="/images/loading_bar.gif"/></div>
<script type="text/javascript">
(function($) {
    $(function() {
        if (typeof $.fn.tempUserSelect == "undefined" && typeof window.loadingUS == "undefined") {
            window.loadingUS = true;
            $.getScript("/scripts/tempUserSelect.js", function() {
                $(".<?php echo $name;?>").tempUserSelect("<?php echo $this->_type;?>");
                delete window.loadingUS;
            });
        } else {
            $.delayed<?php echo $name ?> = function() {
                if (typeof window.loadingUS == "undefined" && typeof $.fn.tempUserSelect != "undefined") {
                    $(".<?php echo $name;?>").tempUserSelect("<?php echo $this->_type;?>");
                } else {
                    setTimeout(function() {$.delayed<?php echo $name?>();}, 200);
                }
            };
            $.delayed<?php echo $name?>();
        }
    })
}) (jQuery);
</script>
        <?php
            $out .= ob_get_clean();
        }
        $out .= '</div>';

        return $out;
    }

    public function __toString()
    {
        return $this->render();
    }
}
