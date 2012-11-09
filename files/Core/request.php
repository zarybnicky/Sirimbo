<?php
class Request {
	private static $uri;
	private static $url;
	private static $url_parts;
	private static $url_parts_literal;
	
	private static $section;
	private static $action;
	private static $id;
	private static $referer;

	public static function setURI($uri) {
		Request::$uri = $uri;
	}
	public static function setURL($url) {
		Request::$url = $url;
		Request::$url_parts = explode('/', $url);
		
		$parts = Request::$url_parts;
		foreach($parts as $key => $part)
			if(is_numeric($part))
				unset($parts[$key]);
		Request::$url_parts_literal = array_values($parts);
		
		for($i = count(Request::$url_parts) - 1; $i >= 0; $i--) {
			if(is_numeric(Request::$url_parts[$i])) {
				$id = Request::$url_parts[$i];
				if(!is_numeric(Request::$url_parts[$i - 1])) {
					$action = Request::$url_parts[$i - 1];
					break;
				}
			}
		}
		Request::$id = isset($id) ? $id : false;
		Request::$action = isset($action) ? $action :
			Request::$url_parts[count(Request::$url_parts) - 1];
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
	public static function getLiteralURL($default = '') {
		if(empty(Request::$url_parts_literal) || Request::$url_parts_literal[0] == '')
			return $default;
		return implode('/', Request::$url_parts_literal);
	}
	public static function getSection($default = '') {
		return isset(Request::$url_parts[0]) ? Request::$url_parts[0] : $default;
	}
	public static function getPage() {
		return isset(Request::$url_parts_literal[1]) ?
			implode('/', array_slice(Request::$url_parts_literal, 1)) : '';
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