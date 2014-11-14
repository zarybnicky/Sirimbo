<?php
class Novinky
{
    private $_user;
    private $_current;
    private static $_format = array(
        'akce' => array(
                'add' => 'Byla přidána nová akce, "<a href="%s">%s</a>".',
                'edit' => 'Akce "<a href="%s">%s</a>" byla upravena.',
                'remove' => 'Akce "%s" byla zrušena.'
        ),
        'clanek' => array(
                'add' => 'Byl přidán nový článek, "<a href="%s">%s</a>".',
                'edit' => 'Článek "<a href="%s">%s</a>" byl upraven.',
                'remove' => 'Článek "%s" byl odstraněn.'
        ),
        'video' => array(
                'add' => 'Bylo přidáno nové video, "<a href="%s">%s</a>".',
                'edit' => 'Video "<a href="%s">%s</a>" bylo upraveno.',
                'remove' => 'Video "%s" bylo odstraněno.'
        ),
        'dokumenty' => array(
                'add' => 'Byl přidán nový dokument, "<a href="%s">%s</a>".',
                'edit' => 'Dokument "<a href="%s">%s</a>" byl upraven.',
                'remove' => 'Dokument "%s" byl odstraněn.'
        ),
        'galerie' => 'Galerie "%s" byla upravena.',
        'nabidka' => array(
                'add' => 'Byla přidána nová <a href="%s">nabídka</a> od %s do %s s trenérem %s',
                'edit' => '<a href="%s">Nabídka</a> od %s do %s s trenérem %s byla upravena.',
                'remove' => 'Nabídka od %s do %s s trenérem %s byla zrušena.'
        ),
        'nastenka' => array(
                'add' => 'Byl přidán nový <a href="%s">příspěvek</a> na nástěnku',
                'edit' => null,
                'remove' => null
        ),
        'rozpis' => array(
                'add' => 'Byl přidán nový <a href="%s">rozpis</a> dne %s s trenérem %s.',
                'edit' => '<a href="%s">Rozpis</a> dne %s s trenérem %s byl upraven.',
                'remove' => 'Rozpis dne %s s trenérem %s byl odstraněn.'
        ),
    );

    public function __construct($userID) {
        $this->_user = $userID;
        $this->_current = null;
    }

    public function akce() {
        $this->_current = 'akce';
        return $this;
    }
    public function clanek() {
        $this->_current = 'clanek';
        return $this;
    }
    public function video() {
        $this->_current = 'video';
        return $this;
    }
    public function dokumenty() {
        $this->_current = 'dokumenty';
        return $this;
    }
    public function galerie() {
        $this->_current = 'galerie';
        return $this;
    }
    public function nabidka() {
        $this->_current = 'nabidka';
        return $this;
    }
    public function nastenka() {
        $this->_current = 'nastenka';
        return $this;
    }
    public function rozpis() {
        $this->_current = 'rozpis';
        return $this;
    }

    public function add() {
        $this->_insert('add', func_get_args());
    }
    public function edit() {
        $this->_insert('edit', func_get_args());
    }
    public function remove() {
        $this->_insert('remove', func_get_args());
    }

    private function _insert($action, $parameters) {
        $format = is_array(self::$_format[$this->_current]) ?
            self::$_format[$this->_current][$action] :
            self::$_format[$this->_current];

        if ($format === null) {
            return;
        }

        $text = call_user_func_array('sprintf', array_merge(array($format), $parameters));
        DBNovinky::addNovinka($this->_user, $text);
    }
}