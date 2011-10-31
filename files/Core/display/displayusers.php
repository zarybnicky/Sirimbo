<?php
class DisplayUsers {
	public static $constant = 1;
	public static $plaintext = 2;
	public static $fulltext = 3;
	
	private static $plaintextTable = array(
		L_UNCONFIRMED	=> "unconfirmed",
		L_HOST			=> "host",
		L_USER			=> "user",
		L_EDITOR		=> "editor",
		L_TRENER		=> "trener",
		L_ADMIN			=> "admin",
		L_SADMIN		=> "sadmin"
	);
	
	private static $fulltextTable = array(
		L_UNCONFIRMED	=> "Nepotvrzený",
		L_HOST			=> "Host",
		L_USER			=> "Člen",
		L_EDITOR		=> "Editor",
		L_TRENER		=> "Trenér",
		L_ADMIN			=> "Administrátor",
		L_SADMIN		=> "Superuživatel"
	);
	
	public static function transformUserLevel($value, $from, $to) {
		switch($from) {
			case DisplayUsers::$constant:
				switch($to) {
					case DisplayUsers::$plaintext:
						return DisplayUsers::$plaintextTable[$value];
						break;
					
					case DisplayUsers::$fulltext:
						return DisplayUsers::$fulltextTable[$value];
						break;
				}
				break;
			
			case DisplayUsers::$plaintext:
				switch($to) {
					case DisplayUsers::$constant:
						return array_search($value, DisplayUsers::$plaintextTable);
						break;
					
					case DisplayUsers::$fulltext:
						return DisplayUsers::$fulltextTable[
							array_search($value, DisplayUsers::$plaintextTable)
						];
						break;
				}
				break;
			
			case DisplayUsers::$fulltext:
				switch($to) {
					case DisplayUsers::$constant:
						return array_search($value, DisplayUsers::$fulltextTable);
						break;
					
					case DisplayUsers::$plaintext:
						return DisplayUsers::$plaintextTable[
							array_search($value, DisplayUsers::$fulltextTable)
						];
						break;
				}
				break;
		}
	}
}