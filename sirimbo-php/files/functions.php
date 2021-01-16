<?php
function array_for(array $arr, callable $fn)
{
    return array_map($fn, $arr);
}
