<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class UserSelect extends Partial
{
    private $file = 'src/library/Template/Helper/UserSelect.tpl';

    private $name         = 'user';
    private $keyId        = 'u_id';
    private $keyJmeno     = 'u_jmeno';
    private $keyPrijmeni  = 'u_prijmeni';
    private $keyBirthDate = 'u_narozeni';
    private $data         = array();
    private $type         = 'user';
    private $showTempForm = true;

    public function __construct($name = null)
    {
        if ($name !== null) {
            $this->name($name);
        }
    }

    public function setName($name)
    {
        $this->name = $name;

        return $this;
    }
    public function setKeyId($key)
    {
        $this->keyId = $key;

        return $this;
    }

    public function setKeyJmeno($key)
    {
        $this->keyJmeno = $key;

        return $this;
    }

    public function setKeyPrijmeni($key)
    {
        $this->keyPrijmeni = $key;

        return $this;
    }

    public function setKeyBirthDate($key)
    {
        $this->keyBirthDate = $key;

        return $this;
    }

    public function setType($type)
    {
        if ($type == 'user' || $type == 'par') {
            $this->type = $type;
        }
        return $this;
    }

    public function showTemporaryUserForm($value)
    {
        $this->showTempForm = (bool) $value;

        return $this;
    }

    public function addUser($id, $fullName, $birthDate)
    {
        list($year, $month, $day) = explode('-', $birthDate);

        $this->data[] = array(
            'id'   => $id,
            'name' => $fullName,
            'year' => $year
        );
    }

    public function setUsers(array $users, $overwrite = false)
    {
        if ($overwrite) {
            $this->data[] = array();
        }

        foreach ($users as $item) {
            $this->addUser(
                $item[$this->keyId],
                $item[$this->keyPrijmeni] . ', ' . $item[$this->keyJmeno],
                $item[$this->keyBirthDate]
            );
        }
        return $this;
    }

    public function render()
    {
        $name = 'userselect' . rand(0, 1024);

        $dateSelect = new Date($this->twigEnvironment, 'narozeni');

        return $this->renderTemplate(
            $this->file,
            array(
                'divClass'     => $name,
                'name'         => $this->name,
                'date'         => $this->data,
                'selected'     => post($this->name) === null ? false : '' . post($this->name),
                'showTempForm' => $this->showTempForm,
                'dateSelect'   => $dateSelect
            )
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
