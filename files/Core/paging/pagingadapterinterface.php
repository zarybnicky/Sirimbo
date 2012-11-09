<?php
interface PagingAdapterInterface {
	function page($offset, $length, $options = '');
	function count();
}