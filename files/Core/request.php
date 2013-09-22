<?php
class Request {
	private static $default = 'home';
	private static $uri;
	private static $url;
	private static $url_canonical;
	private static $url_parts;
	private static $url_parts_literal;
	
	private static $section;
	private static $action;
	private static $id;
	private static $referer;

	public static function setDefault($default) {
		Request::$default = $default;
	}
	public static function setURI($uri) {
		Request::$uri = $uri;
	}
	public static function setURL($url) {
		Request::$url = $url;
		$parts = explode('/', $url);
		
		foreach($parts as $key => $part) {		//remove double slashes
			if($part === '') {
				unset($parts[$key]);
			}
		}
		$parts = array_values($parts);
		Request::$url_parts = $parts;			//raw url parts
		
		foreach($parts as $key => $part)
			if(is_numeric($part))
				unset($parts[$key]);			//make canonical name, usable for finding controllers
		
		$parts = array_values($parts);
		Request::$url_parts_literal = $parts;
		
		for($i = count(Request::$url_parts) - 1; $i >= 0; $i--) {
			if(is_numeric(Request::$url_parts[$i])) {
				$id = Request::$url_parts[$i];	//find last number, set it as the ID for most regular actions
				if(isset(Request::$url_parts[$i - 1]) && !is_numeric(Request::$url_parts[$i - 1])) {
					$action = Request::$url_parts[$i - 1];		//dilemma: action before or after id???
					break;
				}
			}
		}
		Request::$id = isset($id) ? $id : null;
		Request::$action = isset($action) ? $action :			//if previous code didn't find a viable action, use the last string in array
			(!empty(Request::$url_parts_literal) ?
				Request::$url_parts_literal[count(Request::$url_parts_literal) - 1] : null);
	}

	public static function getURI() {
		return Request::$uri;
	}
	public static function getURL() {
		return Request::$url;
	}
	public static function getURLParts() {
		return Request::$url_parts;
	}
	public static function getLiteralURL() {
		if(empty(Request::$url_parts_literal) || Request::$url_parts_literal[0] == '')
			return Request::$default;
		return implode('/', Request::$url_parts_literal);
	}
	public static function getSection() {
		return isset(Request::$url_parts[0]) ? Request::$url_parts[0] : Request::$default;
	}
	public static function getCanonical() {
		return Request::getLiteralURL();
	}
	public static function getAction() {
		return Request::$action;
	}
	public static function getID() {
		return Request::$id;
	}
	
	public static function setReferer($referer) {
		Request::$referer = $referer;
	}
	public static function getReferer() {
		return Request::$referer;
	}
}
?>