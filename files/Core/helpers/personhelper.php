<?php
class PersonHelper
{
    protected $id;
    protected $name;

    public function person($user)
    {
        $this->id = $user['u_id'];
        $this->name = $user['u_jmeno'] . ' ' . $user['u_prijmeni'];
        $this->nameR = $user['u_prijmeni'] . ', ' . $user['u_jmeno'];
        return $this;
    }

    public function render()
    {
        return (string) new Tag(
            'a',
            array('href' => '/member/clenove/' . $this->id),
            new Tag(
                'img',
                array(
                    'src' => '/style/person-small.png',
                    'alt' => $this->name,
                    'style' => 'margin-bottom:-2px'
                )
            ),
            '&nbsp;' . $this->nameR
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}