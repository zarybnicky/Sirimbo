<?php
namespace TKOlomouc\Utility;

use TKOlomouc\Model\DBNovinky;

class Novinky
{
    private $userId;
    private $currentCategory;
    private static $formatStrings = array(
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
        'ankety' => array(
            'add' => 'Byla přidána nová anketa, "%s".',
            'edit' => 'Anketa "%s" byla upravena.',
            'remove' => 'Anketu "%s" byla zrušena.'
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
            'remove' => '<a href="%s">Nabídka</a> od %s do %s s trenérem %s byla zrušena.'
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

    public function __construct($userID)
    {
        $this->userId = $userID;
        $this->currentCategory = null;
    }

    public function akce()
    {
        $this->currentCategory = 'akce';
        return $this;
    }
    public function clanek()
    {
        $this->currentCategory = 'clanek';
        return $this;
    }
    public function video()
    {
        $this->currentCategory = 'video';
        return $this;
    }
    public function ankety()
    {
        $this->currentCategory = 'ankety';
        return $this;
    }
    public function dokumenty()
    {
        $this->currentCategory = 'dokumenty';
        return $this;
    }
    public function galerie()
    {
        $this->currentCategory = 'galerie';
        return $this;
    }
    public function nabidka()
    {
        $this->currentCategory = 'nabidka';
        return $this;
    }
    public function nastenka()
    {
        $this->currentCategory = 'nastenka';
        return $this;
    }
    public function rozpis()
    {
        $this->currentCategory = 'rozpis';
        return $this;
    }

    public function add($_)
    {
        $this->insert('add', func_get_args());
    }
    public function edit($_)
    {
        $this->insert('edit', func_get_args());
    }
    public function remove($_)
    {
        $this->insert('remove', func_get_args());
    }

    private function insert($action, $parameters)
    {
        $format = is_array(self::$formatStrings[$this->currentCategory])
            ? self::$formatStrings[$this->currentCategory][$action]
            : self::$formatStrings[$this->currentCategory];

        if ($format === null) {
            return;
        }

        $text = call_user_func_array('sprintf', array_merge(array($format), $parameters));
        DBNovinky::addNovinka($this->userId, $text);
    }
}