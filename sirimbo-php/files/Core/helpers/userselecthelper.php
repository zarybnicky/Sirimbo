<?php
class UserSelectHelper
{
    private $_name;
    private $_idIndex;
    private $_jmeno;
    private $_prijmeni;
    private $_users;
    private $_type;
    private $_selected;

    public function userSelect($name = null)
    {
        $this->_defaultValues();
        if ($name !== null) {
            return $this->name($name);
        }
        return $this;
    }

    private function _defaultValues()
    {
        $this->_name = "user";
        $this->_idIndex = "u_id";
        $this->_jmeno = "u_jmeno";
        $this->_prijmeni = "u_prijmeni";
        $this->_users = [];
        $this->_type = 'user';
    }

    public function name($name)
    {
        $this->_name = $name;
        return $this;
    }

    public function idVar($idVar)
    {
        $this->_idIndex = $idVar;
        return $this;
    }

    public function jmeno($jmeno)
    {
        $this->_jmeno = $jmeno;
        return $this;
    }

    public function prijmeni($prijmeni)
    {
        $this->_prijmeni = $prijmeni;
        return $this;
    }

    public function users(array $users)
    {
        if (is_array($users)) {
            $this->_users = $users;
        }
        return $this;
    }

    public function type($type)
    {
        if ($type == 'user' || $type == 'par') {
            $this->_type = $type;
        }
        return $this;
    }

    public function set($value)
    {
        $this->_selected = $value;
        return $this;
    }

    public function render()
    {
        $name = 'userselect' . rand(0, 1024);
        $selected = $this->_selected !== null ? $this->_selected : '';

        $out = '<div class="' . $name . '">' . "\n";
        $out .= '<select class="form-control select2" name="' . $this->_name . '">' . "\n";
        if (!$selected) {
            $out .= '<option value="0" selected="selected">--- žádný ---</option>' . "\n";
        } else {
            $out .= '<option value="0">--- žádný ---</option>' . "\n";
        }

        foreach ($this->_users as $user) {
            $id = $user[$this->_idIndex];
            $out .= '<option value="' . $id;
            $out .= $id == $selected ? '" selected="selected">' : '">';
            $out .= $user[$this->_prijmeni] . ', ' . $user[$this->_jmeno];
            if (isset($user['u_narozeni'])) {
                $out .= ', ' . explode('-', $user['u_narozeni'])[0];
            }
            $out .= '</option>' . "\n";
        }
        $out .= '</select>' . "\n";
        $out .= '</div>';

        return $out;
    }

    public function __toString()
    {
        return $this->render();
    }
}
