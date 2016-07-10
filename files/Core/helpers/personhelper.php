<?php
class PersonHelper
{
    protected $id;
    protected $name;

    public function person($user)
    {
        $this->id = $user['u_id'];
        $this->name = $user['u_jmeno'] . ' ' . $user['u_prijmeni'];
    }

    public function render()
    {
        return (string) new Tag(
            'a',
            array('href' => '/member/clenove/' . $item['u_id']),
            new Tag(
                'img',
                array(
                    'src' => '/style/person-small.png',
                    'alt' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'style' => 'margin-bottom:-2px'
                )
            ),
            '&nbsp;' . $item['u_prijmeni'] . ', ' . $item['u_jmeno']
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}