<?php
trait HelperTrait
{
    public function date(...$args)
    {
        return new DateHelper(...$args);
    }
}
