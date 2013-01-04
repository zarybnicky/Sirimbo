<?php
interface PagingAdapterInterface {
	function page($offset, $length, $options = null);
	function count($options = null);
}