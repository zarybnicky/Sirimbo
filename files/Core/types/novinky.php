<?php
class Novinky {
	private $user;
	private $current;
	private static $format = array(
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
	
	function __construct($userID) {
		$this->user = $userID;
		$this->current = null;
	}
	
	function akce() {
		$this->current = 'akce';
		return $this;
	} function clanek() {
		$this->current = 'clanek';
		return $this;
	}function video() {
		$this->current = 'video';
		return $this;
	} function ankety() {
		$this->current = 'ankety';
		return $this;
	} function dokumenty() {
		$this->current = 'dokumenty';
		return $this;
	} function galerie() {
		$this->current = 'galerie';
		return $this;
	} function nabidka() {
		$this->current = 'nabidka';
		return $this;
	} function nastenka() {
		$this->current = 'nastenka';
		return $this;
	} function rozpis() {
		$this->current = 'rozpis';
		return $this;
	}
	
	function add($_) {
		$this->insert('add', func_get_args());
	} function edit($_) {
		$this->insert('edit', func_get_args());
	} function remove($_) {
		$this->insert('remove', func_get_args());
	}
	
	private function insert($action, $parameters) {
		$format = is_array(self::$format[$this->current]) ? self::$format[$this->current][$action] : self::$format[$this->current];
		if($format === null)
			return;
		
		$text = call_user_func_array('sprintf', array_merge(array($format), $parameters));
		DBNovinky::addNovinka($this->user, $text);
	}
}